//! The schema: what types exist, what children they allow, and where those go.
//!
//! Section references are to `docs/storage-layout.md`.

use std::collections::HashMap;
use std::path::Path;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use crate::entity::{Entity, EntityLoader};
use crate::placement::{Edge, Layout};
use inscenerator_xfs::Xfs;

/// Rules for discovering and validating child entities.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    /// Regex pattern to match the child entity name.
    pub name_regex: String,
    /// The expected entity type for matching children.
    pub node_type: String,
    /// Whether at least one child matching this rule must exist. Reported as a finding,
    /// not enforced (§7.3).
    #[serde(default)]
    pub required: bool,
    /// Whether more than one child may match this rule. Reported, not enforced (§7.3).
    #[serde(default)]
    pub multiple: bool,
    /// Which edge children matching this rule attach on — `disk(parent)/name` for
    /// [`Edge::Slash`], `disk(parent).name` for [`Edge::Dot`]. §3. Defaults to slash.
    #[serde(default)]
    pub edge: Edge,
}

/// Description of an entity type, including its allowed children.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EntityTypeDescription {
    /// The name of the entity type.
    pub name: String,
    /// List of rules for child entities.
    pub children: Vec<ChildEntityRules>,
    /// Whether to allow additional children not covered by the rules.
    pub allow_additional: bool,
    /// Regexes matched against child names; a matching name produces no child at all,
    /// and is not an error (e.g. non-entity tool directories). §7.4.
    #[serde(default)]
    pub ignore: Vec<String>,
    /// Where this type's content and sidecar live. `None` inherits from the parent
    /// *instance* this type was loaded under (§2.1). The root is always
    /// [`Layout::Inside`].
    #[serde(default)]
    pub layout: Option<Layout>,
}

/// A child rule with its regex compiled, and its position in its type's rule list.
///
/// The index identifies the rule in findings — §4.2 observes the edge *per rule*, so a
/// parent whose two rules sit on different edges is conforming, while one rule whose
/// children straddle both edges is not.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub rule: ChildEntityRules,
    pub index: usize,
    re: regex::Regex,
}

impl CompiledRule {
    pub fn is_match(&self, name: &str) -> bool {
        self.re.is_match(name)
    }
}

/// An entity type with every regex it owns compiled. Built once, when the schema is.
#[derive(Debug, Clone)]
pub struct CompiledType {
    pub desc: EntityTypeDescription,
    pub rules: Vec<CompiledRule>,
    ignore: Vec<regex::Regex>,
}

/// What a type's rules say about one child name.
///
/// [`CompiledType::match_child`] is the *only* implementation of rule matching; every
/// site that needs to know a child's type or edge goes through it, so no two callers can
/// reach different conclusions about the same name.
#[derive(Debug)]
pub enum ChildMatch<'a> {
    /// Matched an `ignore` regex. Produces no child, and is not an error. §7.4.
    Ignored,
    /// Matched a child rule, which supplies the type and the intended edge.
    Matched(&'a CompiledRule),
    /// No rule matched, but `allow_additional` is set — so no declared type and no
    /// declared edge either. §7.2.
    Additional,
    /// No rule matched and `allow_additional` is not set.
    Unexpected,
}

impl CompiledType {
    /// Classifies one child name. `ignore` wins over the rules; the first matching rule
    /// wins over later ones.
    pub fn match_child(&self, name: &str) -> ChildMatch<'_> {
        if self.ignore.iter().any(|re| re.is_match(name)) {
            return ChildMatch::Ignored;
        }
        if let Some(r) = self.rules.iter().find(|r| r.is_match(name)) {
            return ChildMatch::Matched(r);
        }
        if self.desc.allow_additional {
            ChildMatch::Additional
        } else {
            ChildMatch::Unexpected
        }
    }
}

fn compile(desc: &EntityTypeDescription) -> anyhow::Result<CompiledType> {
    fn re(pattern: &str, field: &str, type_name: &str) -> anyhow::Result<regex::Regex> {
        regex::Regex::new(pattern).map_err(|e| {
            anyhow!("Invalid regex {:?} in {}.{}: {}", pattern, type_name, field, e)
        })
    }

    let rules = desc
        .children
        .iter()
        .enumerate()
        .map(|(index, rule)| {
            Ok(CompiledRule {
                rule: rule.clone(),
                index,
                re: re(&rule.name_regex, "children.name_regex", &desc.name)?,
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let ignore = desc
        .ignore
        .iter()
        .map(|p| re(p, "ignore", &desc.name))
        .collect::<anyhow::Result<Vec<_>>>()?;

    Ok(CompiledType { desc: desc.clone(), rules, ignore })
}

/// A collection of entity type descriptions.
#[derive(Default, Clone, Debug)]
pub struct Schema {
    /// Map of entity type names to their descriptions.
    /// Private so it cannot drift from `compiled`: a type inserted here without its
    /// regexes compiled would be found by `get_entity_type` and missed by `compiled`,
    /// which is exactly the disagreement one matcher exists to prevent. Read it through
    /// [`Self::entity_types`]; write it through [`Self::add_entity_type`].
    entity_types: HashMap<String, EntityTypeDescription>,
    /// The same types with their regexes compiled, kept in step by `add_entity_type`.
    /// Separate because `EntityTypeDescription` is `PartialEq + Serialize` and
    /// `regex::Regex` is neither.
    compiled: HashMap<String, CompiledType>,
}

#[derive(Deserialize)]
struct RawEntityTypeDescription {
    #[serde(default)]
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
    #[serde(default)]
    pub ignore: Vec<String>,
    #[serde(default)]
    pub layout: Option<Layout>,
}

impl Schema {
    /// Creates a new, empty schema.
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets an entity type description by name.
    ///
    /// # Errors
    ///
    /// Returns an error if the entity type is not found in the schema.
    /// Every declared type, by name.
    pub fn entity_types(&self) -> &HashMap<String, EntityTypeDescription> {
        &self.entity_types
    }

    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// The compiled form of a type — what every rule-matching site should ask for.
    ///
    /// # Errors
    ///
    /// Returns an error if the entity type is not found in the schema.
    pub fn compiled(&self, entity_type: &str) -> anyhow::Result<&CompiledType> {
        self.compiled
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// Adds a new entity type description to the schema, compiling its regexes.
    ///
    /// # Errors
    ///
    /// Returns an error if any `name_regex` or `ignore` pattern is not a valid regex.
    /// Compiling here rather than at each use is what makes a bad pattern a single
    /// schema-construction failure instead of an error some callers raise and others
    /// silently skip.
    pub fn add_entity_type(&mut self, description: EntityTypeDescription) -> anyhow::Result<()> {
        let compiled = compile(&description)?;
        self.compiled.insert(description.name.clone(), compiled);
        self.entity_types.insert(description.name.clone(), description);
        Ok(())
    }

    /// Loads a schema from a TOML file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read, or if [`load_from_str`](Self::load_from_str)
    /// rejects its contents.
    pub fn load_from_file(fs: &dyn Xfs, path: &Path) -> anyhow::Result<Self> {
        let content = crate::entity::utils::try_load_file_as_string(fs, path)?
            .ok_or_else(|| anyhow!("Schema file not found at {:?}", path))?;
        Self::load_from_str(&content)
    }

    /// Loads a schema from the contents of a `schema.toml`.
    ///
    /// This is the whole of schema construction — [`load_from_file`](Self::load_from_file)
    /// only reads the bytes and hands them here. Use it directly to embed a schema in a
    /// binary, or to build one from a source that is not a file.
    ///
    /// # Errors
    ///
    /// Returns an error if the TOML does not parse, or if any `name_regex` or `ignore`
    /// pattern is not a valid regex.
    pub fn load_from_str(content: &str) -> anyhow::Result<Self> {
        let raw_entities: HashMap<String, RawEntityTypeDescription> = toml::from_str(content)?;
        let mut schema = Schema::new();
        for (name, raw) in raw_entities {
            schema.add_entity_type(EntityTypeDescription {
                name,
                children: raw.children,
                allow_additional: raw.allow_additional,
                ignore: raw.ignore,
                layout: raw.layout,
            })?;
        }
        Ok(schema)
    }
}

/// Loads a schema and the root entity from a directory.
///
/// Assumes the schema is in a file named 'schema.toml' in the root directory.
///
/// # Errors
///
/// Returns an error if the schema or root entity cannot be loaded.
pub fn load_schema_and_root(fs: &dyn Xfs, root_path: &Path) -> anyhow::Result<(Schema, Entity)> {
    let schema_path = root_path.join("schema.toml");
    let schema = Schema::load_from_file(fs, &schema_path)?;

    let mut loader = EntityLoader::new();
    loader.schema = schema.clone();

    let root = loader.try_load_root(fs, root_path, "Auto")?
        .ok_or_else(|| anyhow!("Root entity not found in {:?}", root_path))?;

    Ok((schema, root))
}

#[cfg(test)]
mod tests {
    use super::*;
    use inscenerator_xfs::mockfs;
    use std::path::PathBuf;

    fn create_file_with_content<P: Into<PathBuf>, F: AsRef<Path>>(
        fs: &mut mockfs::MockFS,
        dir: P,
        filename: F,
        content: &str,
    ) {
        let p = dir.into();
        fs.create_dir_all(&p).unwrap();
        fs.add_r(&p.join(filename), content.as_bytes().to_vec())
            .unwrap();
    }

    /// A parent whose two rules sit on opposite edges, plus a leaf that takes additional
    /// children. Between them these cover every `ChildMatch` outcome.
    const EDGE_SCHEMA: &str = r#"
[Chapter]
allow_additional = false
layout = "parallel"
ignore = ['^\.gitkeep$', '^README\.md$']
[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"
[[Chapter.children]]
name_regex = '^notes$'
node_type = "Note"
edge = "dot"

[Section]
allow_additional = true
children = []

[Note]
allow_additional = false
children = []
"#;

    /// §2 / §3: the two declarations this whole design rests on — `layout` on the type,
    /// `edge` on the parent's rule.
    #[test]
    fn types_declare_a_layout_and_rules_declare_an_edge() {
        let s = Schema::load_from_str(EDGE_SCHEMA).unwrap();
        let chapter = &s.entity_types()["Chapter"];
        assert_eq!(chapter.layout, Some(Layout::Parallel));
        assert_eq!(chapter.children[0].edge, Edge::Slash);
        assert_eq!(chapter.children[1].edge, Edge::Dot);
        // Section omits `layout`, so it inherits from the parent instance at load time.
        assert_eq!(s.entity_types()["Section"].layout, None);
    }

    /// §2 / §3: what a schema that declares nothing optional gets.
    #[test]
    fn omitted_declarations_take_their_documented_defaults() {
        let s = Schema::load_from_str(
            r#"
[T]
allow_additional = false
[[T.children]]
name_regex = "x"
node_type = "T"
"#,
        )
        .unwrap();
        let t = &s.entity_types()["T"];
        assert_eq!(t.layout, None, "no layout means inherit from the parent instance");
        assert!(t.ignore.is_empty());
        assert_eq!(t.children[0].edge, Edge::Slash, "dot is opted into, never defaulted");
        assert!(!t.children[0].required);
        assert!(!t.children[0].multiple);
    }

    /// §7.1: one matcher decides every child name's fate. `Unexpected` vs
    /// `Additional` is the type's `allow_additional`, which is why two types appear here.
    #[test]
    fn match_child_dispatches_to_the_right_outcome() {
        fn outcome(m: ChildMatch<'_>) -> String {
            match m {
                ChildMatch::Ignored => "ignored".to_string(),
                ChildMatch::Matched(r) => format!("matched[{}]", r.index),
                ChildMatch::Additional => "additional".to_string(),
                ChildMatch::Unexpected => "unexpected".to_string(),
            }
        }

        let s = Schema::load_from_str(EDGE_SCHEMA).unwrap();
        for (parent_type, name, expected) in [
            ("Chapter", ".gitkeep", "ignored"),
            ("Chapter", "README.md", "ignored"),
            ("Chapter", "010-intro", "matched[0]"),
            ("Chapter", "notes", "matched[1]"),
            ("Chapter", "stray", "unexpected"), // allow_additional = false
            ("Section", "stray", "additional"), // allow_additional = true
        ] {
            let got = outcome(s.compiled(parent_type).unwrap().match_child(name));
            assert_eq!(got, expected, "{} child {:?}", parent_type, name);
        }
    }

    /// §7.1: a match hands back the rule, which is where the child's type and edge
    /// come from — the caller never re-derives them.
    #[test]
    fn a_match_carries_the_rules_type_edge_and_index() {
        let s = Schema::load_from_str(EDGE_SCHEMA).unwrap();
        match s.compiled("Chapter").unwrap().match_child("notes") {
            ChildMatch::Matched(r) => {
                assert_eq!(r.rule.node_type, "Note");
                assert_eq!(r.rule.edge, Edge::Dot);
                assert_eq!(r.index, 1, "the index identifies the rule in findings");
            }
            other => panic!("expected Matched, got {:?}", other),
        }
    }

    /// §7.4: `ignore` is a regex list, matched the same way `children` is.
    #[test]
    fn ignore_entries_are_regexes() {
        let s = Schema::load_from_str(
            r#"
[T]
allow_additional = false
ignore = ['^\.']
children = []
"#,
        )
        .unwrap();
        let t = s.compiled("T").unwrap();
        assert!(matches!(t.match_child(".hidden"), ChildMatch::Ignored));
        assert!(matches!(t.match_child("visible"), ChildMatch::Unexpected));
    }

    /// Every regex is compiled once, when the schema is built. A bad pattern can no
    /// longer surface as an error from one call site and be silently skipped by another,
    /// because there is no schema for the two to disagree over.
    #[test]
    fn invalid_regexes_are_rejected_when_the_schema_is_built() {
        for (field, bad_pattern, src) in [
            (
                "children",
                "[",
                r#"
[T]
allow_additional = false
[[T.children]]
name_regex = "["
node_type = "T"
"#,
            ),
            (
                "ignore",
                "(",
                r#"
[T]
allow_additional = false
ignore = ["("]
children = []
"#,
            ),
        ] {
            let err = match Schema::load_from_str(src) {
                Err(e) => e.to_string(),
                Ok(_) => panic!("{}: a bad regex must not build a schema", field),
            };
            assert!(err.contains("Invalid regex"), "{}: {}", field, err);
            assert!(err.contains(bad_pattern), "{}: must name the pattern: {}", field, err);
        }
    }

    #[test]
    fn test_load_schema_from_toml() {
        let schema = Schema::load_from_str(
            r#"
[Project]
allow_additional = false
[[Project.children]]
name_regex = "^[0-9]+_"
node_type = "Chapter"
required = false
multiple = true

[Chapter]
allow_additional = true
children = []
"#,
        )
        .unwrap();
        assert_eq!(schema.entity_types().len(), 2);
        assert_eq!(schema.entity_types()["Project"].name, "Project");
        assert_eq!(schema.entity_types()["Project"].children.len(), 1);
        assert_eq!(schema.entity_types()["Project"].children[0].node_type, "Chapter");
        assert_eq!(schema.entity_types()["Chapter"].name, "Chapter");
        assert!(schema.entity_types()["Chapter"].allow_additional);
    }

    #[test]
    fn test_load_schema_and_root() {
        let mut fs = mockfs::MockFS::new();
        let schema_toml = r#"
[Project]
allow_additional = false
[[Project.children]]
name_regex = "^[0-9]+_"
node_type = "Chapter"
required = false
multiple = true

[Chapter]
allow_additional = true
children = []
"#;
        create_file_with_content(&mut fs, "project", "schema.toml", schema_toml);
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/010_chap", "content.md", "Chapter content");

        let (schema, root) = load_schema_and_root(&fs, &Path::new("project")).unwrap();
        assert_eq!(schema.entity_types().len(), 2);
        assert_eq!(root.node_type, "Project");
        assert_eq!(root.children.len(), 1);
        assert_eq!(root.children[0].node_type, "Chapter");
    }

    #[test]
    fn test_load_schema_and_root_ignores_schema_toml() {
        let mut fs = mockfs::MockFS::new();
        let schema_toml = r#"
[Project]
allow_additional = true
[[Project.children]]
name_regex = "some_child"
node_type = "Type"
required = false
multiple = true

[Type]
allow_additional = true
children = []
"#;
        create_file_with_content(&mut fs, "project", "schema.toml", schema_toml);
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/some_child", "content.md", "Child content");

        let (_, root) = load_schema_and_root(&fs, &Path::new("project")).unwrap();
        // Should have 1 child (some_child), but NOT schema.toml
        assert_eq!(root.children.len(), 1);
        assert_eq!(root.children[0].path.entries.last().unwrap().to_pathbuf(Path::new("")).to_str().unwrap(), "some_child");
    }

    #[test]
    fn test_load_schema_and_root_ignores_listed_directories() {
        let mut fs = mockfs::MockFS::new();
        let schema_toml = r#"
[Project]
allow_additional = false
ignore = ["booker-data"]
[[Project.children]]
name_regex = "^[0-9]+_"
node_type = "Chapter"
required = false
multiple = true

[Chapter]
allow_additional = true
children = []
"#;
        create_file_with_content(&mut fs, "project", "schema.toml", schema_toml);
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/010_chap", "content.md", "Chapter content");
        create_file_with_content(&mut fs, "project/booker-data", "some_file.txt", "tool data");

        let schema = Schema::load_from_file(&fs, &Path::new("project/schema.toml")).unwrap();
        assert_eq!(schema.entity_types()["Project"].ignore, vec!["booker-data"]);

        let (_, root) = load_schema_and_root(&fs, &Path::new("project")).unwrap();
        assert_eq!(root.children.len(), 1);
        assert_eq!(root.children[0].node_type, "Chapter");
    }
}
