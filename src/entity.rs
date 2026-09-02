use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, Context};
use inscenerator_xfs::Xfs;

use crate::placement::MetaLocation;

// (Almost) Everything is an entity.
//
// An entity has optional content, optional children, a location, and some metadata.
// And a type.
//
// the path, P,  to an entity is composed of "names" either joined by "/" or ".".
// the content for an entity is either at P.md or P/content.md - both existing is an error
// the metadata for an entity is either at P.meta.toml or P/meta.toml - both existing is an error
// The children of an entity are either at P/childname or P.childname
//
// a name is not allowed to contain "/" or "."
//
// The root element is special - its path is empty - and must represent a directory,
// as such it can only have children of the "/" type.

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct EntityPath {
    pub entries: Vec<EntityPathEntry>,
}

impl EntityPath {
    pub fn to_pathbuf(&self, base_path: &Path) -> PathBuf {
        let mut current_path = PathBuf::from(base_path);
        for entry in &self.entries {
            current_path = entry.to_pathbuf(&current_path);
        }
        current_path
    }

    pub fn local_path(&self) -> PathBuf {
        let mut current_path = PathBuf::from("");
        for entry in &self.entries {
            current_path = entry.to_pathbuf(&current_path);
        }
        current_path
    }

    pub fn extend_dot(&self, suffix: &str) -> EntityPath {
        self.extend(EntityPathEntry::Dot(suffix.to_string()))
    }

    pub fn extend_slash<T: ToString>(&self, name: T) -> EntityPath {
        self.extend(EntityPathEntry::Slash(name.to_string()))
    }

    pub fn extend(&self, entry: EntityPathEntry) -> EntityPath {
        let mut new_entries = self.entries.clone();
        new_entries.push(entry);
        EntityPath {
            entries: new_entries,
        }
    }

    pub fn empty() -> EntityPath {
        EntityPath { entries: vec![] }
    }

    pub fn last_name(&self) -> Option<&str> {
        self.entries.last().map(|entry| match entry {
            EntityPathEntry::Slash(name) => name.as_str(),
            EntityPathEntry::Dot(name) => name.as_str(),
        })
    }

    /// Parses the compact string form: `"chapters/ch1.review"` is slash `chapters`,
    /// slash `ch1`, dot `review`. The empty string is the root.
    ///
    /// This is the inverse of the [`Display`](std::fmt::Display) impl, and the string is
    /// also exactly the entity's stem on disk relative to the base path. The round trip
    /// is unambiguous only because a name may not contain an interior `.`.
    ///
    /// A **leading** `.` belongs to the name — `.gitkeep` is one name, not an empty name
    /// with a dot-child `gitkeep`. This mirrors [`Path::file_prefix`], which is how
    /// discovery reads names off disk.
    ///
    /// # Errors
    ///
    /// Returns an error for an empty segment, an empty dot name, or a name of `.` or `..`.
    pub fn parse(s: &str) -> anyhow::Result<EntityPath> {
        fn check(name: &str, whole: &str) -> anyhow::Result<()> {
            if name.is_empty() {
                bail!("Empty name in entity path {:?}", whole);
            }
            if name == "." || name == ".." {
                bail!("Invalid name {:?} in entity path {:?}", name, whole);
            }
            Ok(())
        }

        let mut path = EntityPath::empty();
        if s.is_empty() {
            return Ok(path);
        }
        for segment in s.split('/') {
            if segment.is_empty() {
                bail!("Empty segment in entity path {:?}", s);
            }
            // The first character is always part of the name, even when it is a '.'.
            let mut chars = segment.char_indices();
            chars.next();
            let (head, tail) = match chars.find(|(_, c)| *c == '.').map(|(i, _)| i) {
                Some(i) => (&segment[..i], &segment[i..]),
                None => (segment, ""),
            };
            check(head, s)?;
            path = path.extend_slash(head);
            if !tail.is_empty() {
                for name in tail[1..].split('.') {
                    check(name, s)?;
                    path = path.extend_dot(name);
                }
            }
        }
        Ok(path)
    }
}

impl std::fmt::Display for EntityPath {
    /// The compact string form — see [`EntityPath::parse`]. Always uses `/` as the
    /// separator, so on a platform whose separator differs this is not the same string
    /// as `local_path().display()`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, entry) in self.entries.iter().enumerate() {
            match entry {
                EntityPathEntry::Slash(name) => {
                    if i > 0 {
                        f.write_str("/")?;
                    }
                    f.write_str(name)?;
                }
                EntityPathEntry::Dot(name) => {
                    f.write_str(".")?;
                    f.write_str(name)?;
                }
            }
        }
        Ok(())
    }
}

impl std::str::FromStr for EntityPath {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<EntityPath> {
        EntityPath::parse(s)
    }
}

#[cfg(test)]
mod entity_path_tests {
    use super::*;

    /// §1: the compact string form is exactly the entity's stem on disk relative to the
    /// base, and it round-trips — which holds only because a name has no interior '.'.
    #[test]
    fn string_form_is_the_on_disk_stem_and_round_trips() {
        for spec in [
            "",
            "chapters",
            "chapters/ch1",
            "chapters/ch1.notes",
            "chapters/ch1.notes.draft",
            "chapters/.gitkeep",
            "a.b/c.d",
        ] {
            let path = EntityPath::parse(spec).unwrap();
            assert_eq!(path.to_string(), spec);
            assert_eq!(path.local_path(), PathBuf::from(spec), "on-disk stem of {:?}", spec);
        }
    }

    /// A leading '.' is part of the name, mirroring `Path::file_prefix` — so `.gitkeep`
    /// is one child named `.gitkeep`, which is what §7.3 needs a rule to be able to match.
    #[test]
    fn a_leading_dot_belongs_to_the_name() {
        let path = EntityPath::parse("chapters/.gitkeep").unwrap();
        assert_eq!(path.entries.len(), 2);
        assert_eq!(path.last_name(), Some(".gitkeep"));
    }

    /// §1: forms with no valid reading are rejected rather than silently mangled.
    #[test]
    fn parse_rejects_forms_with_no_valid_reading() {
        for bad in ["/a", "a/", "a//b", "a.", "a..b", ".", "..", "a/.."] {
            assert!(EntityPath::parse(bad).is_err(), "{:?} should not parse", bad);
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum EntityPathEntry {
    Slash(String),
    Dot(String),
}

impl EntityPathEntry {
    pub fn to_pathbuf(&self, base_path: &Path) -> PathBuf {
        match self {
            EntityPathEntry::Slash(name) => base_path.join(name),
            EntityPathEntry::Dot(name) => {
                let current = base_path.file_name().unwrap().to_str().unwrap();
                let new_name = format!("{}.{}", current, name);
                base_path.with_file_name(new_name)
            }
        }
    }

    fn is_slash(&self) -> bool {
        matches!(self, EntityPathEntry::Slash(_))
    }
}

pub(crate) mod utils {
    use std::collections::BTreeSet;
    use yaml_rust::Yaml;

    use super::*;

    pub fn yaml_to_toml(yaml: &Yaml) -> toml::Value {
        match yaml {
            Yaml::Real(s) => {
                if let Ok(f) = s.parse::<f64>() {
                    toml::Value::Float(f)
                } else {
                    toml::Value::String(s.clone())
                }
            }
            Yaml::Integer(i) => toml::Value::Integer(*i),
            Yaml::String(s) => toml::Value::String(s.clone()),
            Yaml::Boolean(b) => toml::Value::Boolean(*b),
            Yaml::Array(a) => toml::Value::Array(a.iter().map(yaml_to_toml).collect()),
            Yaml::Hash(h) => {
                let mut map = toml::map::Map::new();
                for (k, v) in h {
                    let key = match k {
                        Yaml::String(s) => s.clone(),
                        _ => format!("{:?}", k),
                    };
                    map.insert(key, yaml_to_toml(v));
                }
                toml::Value::Table(map)
            }
            Yaml::Null => toml::Value::String("null".to_string()),
            _ => toml::Value::String(format!("{:?}", yaml)),
        }
    }

    pub fn toml_to_yaml(toml: &toml::Value) -> Yaml {
        match toml {
            toml::Value::String(s) => Yaml::String(s.clone()),
            toml::Value::Integer(i) => Yaml::Integer(*i),
            toml::Value::Float(f) => Yaml::Real(f.to_string()),
            toml::Value::Boolean(b) => Yaml::Boolean(*b),
            toml::Value::Datetime(d) => Yaml::String(d.to_string()),
            toml::Value::Array(a) => Yaml::Array(a.iter().map(toml_to_yaml).collect()),
            toml::Value::Table(t) => {
                let mut map = yaml_rust::yaml::Hash::new();
                for (k, v) in t {
                    map.insert(Yaml::String(k.clone()), toml_to_yaml(v));
                }
                Yaml::Hash(map)
            }
        }
    }

    pub fn format_metadata_header(
        metadata: &Metadata,
        header_type: HeaderType,
        sep: Option<&str>,
        content_body: &str,
    ) -> anyhow::Result<String> {
        let mut to_write = String::new();
        match header_type {
            HeaderType::Toml => {
                to_write.push_str("```toml\n");
                to_write.push_str(&toml::to_string(&metadata.value)?);
                to_write.push_str("```\n");
            }
            HeaderType::Yaml => {
                let yaml = toml_to_yaml(&metadata.value);
                let mut out_str = String::new();
                {
                    let mut emitter = yaml_rust::YamlEmitter::new(&mut out_str);
                    emitter.dump(&yaml).unwrap();
                }
                to_write.push_str(&out_str);
                if !out_str.ends_with('\n') {
                    to_write.push_str("\n");
                }
                to_write.push_str("---\n");
            }
        }
        if let Some(s) = sep {
            to_write.push_str(s);
        } else if !content_body.starts_with('\n') {
            to_write.push_str("\n");
        }
        Ok(to_write)
    }

    pub fn find_dot_children(
        fs: &dyn Xfs,
        base_path: &Path,
        entity_path: &EntityPath,
    ) -> anyhow::Result<Vec<EntityPath>> {
        let p = entity_path.to_pathbuf(base_path);
        let Some(p_str) = p.to_str() else {
            bail!("path {p:?} not convertable to string.")
        };
        let Some(entity_dir) = p.parent() else {
            bail!("Entity path {:?} has no parent", p);
        };
        let mut child_names: BTreeSet<String> = BTreeSet::new();

        let p_dot_str = format!("{}.", p_str);
        for de in fs.read_dir(entity_dir)? {
            let de = de?;
            let entry_path = de.path();
            let Some(entry_path_str) = entry_path.to_str() else {
                bail!("child path {entry_path:?} not converable to srting")
            };

            // Now we only want something like entity.suffix (where suffix might contain more '.' characters.)
            // Throw away the start, just keeping the suffix.
            let Some(suffix) = entry_path_str.strip_prefix(&p_dot_str) else {
                continue;
            };

            // We want to skip a couple of special cases
            if suffix == "md" || suffix == "meta.toml" {
                continue;
            }

            // Now if the suffix is of the form 'name.rest' we just want name, but if there is no '.'
            // then we just want 'name'.
            let name = match suffix.split_once('.') {
                Some((name, _)) => name,
                None => suffix,
            };

            child_names.insert(name.to_string());
        }

        Ok(child_names
            .into_iter()
            .map(|name| entity_path.extend(EntityPathEntry::Dot(name)))
            .collect())
    }

    pub fn find_slash_children(
        fs: &dyn Xfs,
        base_path: &Path,
        entity_path: &EntityPath,
    ) -> anyhow::Result<Vec<EntityPath>> {
        let entity_dir = entity_path.to_pathbuf(base_path);
        if !fs.is_dir(&entity_dir) {
            return Ok(vec![]);
        }
        let mut child_names: BTreeSet<String> = BTreeSet::new();

        for de in fs.read_dir(&entity_dir)? {
            let de = de?;
            let entry_path = de.path();
            if let Some(full_filename) = entry_path.file_name() {
                if full_filename == "content.md" || full_filename == "meta.toml" || full_filename == "schema.toml" {
                    continue;
                }
            }
            let Some(name) = entry_path.file_prefix().and_then(|s| s.to_str()) else {
                bail!("Entry path {:?} has no filename", entry_path);
            };
            child_names.insert(name.to_string());
        }
        Ok(child_names
            .into_iter()
            .map(|name| entity_path.extend(EntityPathEntry::Slash(name)))
            .collect())
    }

    pub fn try_load_file_as_string(fs: &dyn Xfs, path: &Path) -> anyhow::Result<Option<String>> {
        if !fs.is_file(path) {
            return Ok(None);
        }
        let mut r = fs.reader(path)?;
        let mut content = String::new();
        use std::io::Read;
        r.read_to_string(&mut content)?;
        Ok(Some(content))
    }

    pub fn try_load_file_as_metadata(
        fs: &dyn Xfs,
        path: &Path,
    ) -> anyhow::Result<Option<Metadata>> {
        let content = try_load_file_as_string(fs, path)?;
        if let Some(c) = content {
            let value: toml::Value = toml::from_str(&c)?;
            Ok(Some(Metadata { value }))
        } else {
            Ok(None)
        }
    }

    pub fn split_out_yaml_front_matter(content: &str) -> Option<(String, String)> {
        if !content.starts_with("---") {
            return None;
        }
        let mut lines = content.split_inclusive('\n');
        let first_line = lines.next()?;
        if first_line.trim_end_matches(['\n', '\r', ' ', '\t']) != "---" {
            return None;
        }
        let mut inner_yaml = String::new();
        let mut end_pos = first_line.len();
        let mut found_end = false;
        for line in lines {
            if line.trim_end_matches(['\n', '\r', ' ', '\t']) == "---" {
                found_end = true;
                end_pos += line.len();
                break;
            }
            inner_yaml.push_str(line);
            end_pos += line.len();
        }

        if found_end {
            let actual_content = content[end_pos..].to_string();
            Some((actual_content, inner_yaml))
        } else {
            None
        }
    }

    pub fn split_out_toml_front_matter(content: &str) -> Option<(String, String, Option<String>)> {
        let thematic_break_re =
            regex::Regex::new(r"^[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:_[ \t]*){3,}|(?:\*[ \t]*){3,})[ \t]*$")
                .unwrap();

        let mut start_pos = 0;
        let mut lines_iter = content.split_inclusive('\n');
        let mut found_start = false;
        for line in lines_iter.by_ref() {
            if line.trim().is_empty() {
                start_pos += line.len();
                continue;
            }
            if line.starts_with("```") {
                let rest = line[3..].trim();
                if rest.is_empty() || rest == "toml" {
                    found_start = true;
                    start_pos += line.len();
                    break;
                }
            }
            return None; // Not a header
        }
        if !found_start {
            return None;
        }

        let mut end_pos = start_pos;
        let mut found_end = false;
        let mut inner_toml = String::new();
        for line in lines_iter.by_ref() {
            if line.starts_with("```") && line[3..].trim().is_empty() {
                found_end = true;
                end_pos += line.len();
                break;
            }
            inner_toml.push_str(line);
            end_pos += line.len();
        }
        if !found_end {
            return None;
        }

        // remove the trailing newline from inner_toml if present
        let inner_toml_trimmed = inner_toml.trim_end_matches(['\n', '\r']).to_string();

        // Find optional separator
        let mut separator: Option<String> = None;
        let mut content_start_pos = end_pos;

        let mut current_gap = String::new();
        let mut temp_lines_iter = content[end_pos..].split_inclusive('\n');

        for line in temp_lines_iter.by_ref() {
            if thematic_break_re.is_match(line.trim_end_matches(['\n', '\r'])) {
                current_gap.push_str(line);
                separator = Some(current_gap);
                content_start_pos = end_pos + separator.as_ref().unwrap().len();
                break;
            }
            if !line.trim().is_empty() {
                break;
            }
            current_gap.push_str(line);
        }

        let actual_content = content[content_start_pos..].to_string();
        Some((actual_content, inner_toml_trimmed, separator))
    }

    pub fn parse_header(content: &str) -> Option<(Metadata, Option<String>, String, HeaderType)> {
        if let Some((actual_content, inner_yaml)) = split_out_yaml_front_matter(content) {
            let docs = yaml_rust::YamlLoader::load_from_str(&inner_yaml).ok()?;
            if !docs.is_empty() {
                let metadata = Metadata {
                    value: yaml_to_toml(&docs[0]),
                };
                return Some((metadata, None, actual_content, HeaderType::Yaml));
            }
        }

        if let Some((actual_content, inner_toml, separator)) = split_out_toml_front_matter(content) {
            let value: toml::Value = toml::from_str(&inner_toml).ok()?;
            let metadata = Metadata { value };
            return Some((metadata, separator, actual_content, HeaderType::Toml));
        }

        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Metadata {
    pub value: toml::Value,
}

impl Metadata {
    pub fn get_vec_of_string(&self, key: &str) -> anyhow::Result<Option<Vec<String>>> {
        let v = self.value.get(key);
        let v = match v {
            None => return Ok(None),
            Some(v) => v,
        };
        let v = v
            .as_array()
            .ok_or_else(|| anyhow!(format!("{} field is not an array", key)))?;
        let v = v
            .iter()
            .map(|v| {
                v.as_str()
                    .ok_or_else(|| anyhow!(format!("non-string entry in {} array", key)))
                    .map(|v| v.to_string())
            })
            .collect::<anyhow::Result<Vec<String>>>()?;
        Ok(Some(v))
    }

    pub fn get_str(&self, key: &str) -> anyhow::Result<Option<String>> {
        let v = match self.value.get(key) {
            None => None,
            Some(v) => Some(String::from(
                v.as_str()
                    .ok_or_else(|| anyhow!("name field is not a string"))?,
            )),
        };
        Ok(v)
    }
}

use crate::schema::{Schema, EntityTypeDescription};

pub struct EntityLoader {
    pub schema: Schema,
}

impl EntityLoader {
    pub fn new() -> EntityLoader {
        EntityLoader {
            schema: Schema::new(),
        }
    }

    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.schema.get_entity_type(entity_type)
    }

    pub fn try_load_entity(
        &self,
        fs: &dyn Xfs,
        base_path: &Path,
        entity_path: &EntityPath,
        entity_type: &str,
    ) -> anyhow::Result<Option<Entity>> {
        // Root element is special
        let is_root = entity_path.entries.is_empty();
        if is_root {
            // Root must be a directory
            let root_path = entity_path.to_pathbuf(base_path);
            if !fs.is_dir(&root_path) {
                bail!("Root entity at {} must be a directory", root_path.display());
            }
        }

        // Try loading the content
        let directory_exists = fs.is_dir(&entity_path.to_pathbuf(base_path));
        let dot_content_file = entity_path.to_pathbuf(base_path).with_added_extension("md");
        let slash_content_file = entity_path.to_pathbuf(base_path).join("content.md");

        let (content_str, is_parallel) = if !is_root {
            if let Some(c) = utils::try_load_file_as_string(fs, &dot_content_file)? {
                if fs.is_file(&slash_content_file) {
                    bail!(
                        "Both {} and {} exist.",
                        dot_content_file.display(),
                        slash_content_file.display()
                    );
                }
                (Some(c), true)
            } else {
                (utils::try_load_file_as_string(fs, &slash_content_file)?, false)
            }
        } else {
            (utils::try_load_file_as_string(fs, &slash_content_file)?, false)
        };

        let (content, metadata_from_content) = if let Some(c) = content_str {
            let (m, a) = utils::parse_header(&c)
                .map(|(m, separator, a, header_type)| {
                    let origin = MetaOrigin::Header { header_type, separator };
                    (Some(MetaSource { origin, state: MetaState::Parsed(m) }), a)
                })
                .unwrap_or((None, c));
            if is_parallel {
                (EntityContent::Parallel(a), m)
            } else {
                (EntityContent::Inside(a), m)
            }
        } else {
            (EntityContent::None, None)
        };

        // Try loading the metadata
        let dot_metadata_file = entity_path
            .to_pathbuf(base_path)
            .with_extension("meta.toml");
        let slash_metadata_file = entity_path.to_pathbuf(base_path).join("meta.toml");
        let dot_metadata = if !is_root {
            utils::try_load_file_as_metadata(fs, &dot_metadata_file)?
        } else {
            None
        };
        let slash_metadata = utils::try_load_file_as_metadata(fs, &slash_metadata_file)?;

        let mut meta_sources = Vec::new();
        if let Some(m) = dot_metadata {
            meta_sources.push(MetaSource {
                origin: MetaOrigin::ParallelSidecar,
                state: MetaState::Parsed(m),
            });
        }
        if let Some(m) = slash_metadata {
            meta_sources.push(MetaSource {
                origin: MetaOrigin::InsideSidecar,
                state: MetaState::Parsed(m),
            });
        }
        if let Some(m) = metadata_from_content {
            meta_sources.push(m);
        }
        // Several sources merge per key (D2); the ambiguity is reported as a finding in
        // Task 6 rather than refused here.
        let metadata = EntityMeta::of(meta_sources);

        // Now get the children!
        let dot_children = if !is_root {
            utils::find_dot_children(fs, base_path, entity_path)?
        } else {
            vec![]
        };
        let slash_children = utils::find_slash_children(fs, base_path, entity_path)?;
        let children = dot_children
            .into_iter()
            .chain(slash_children)
            .collect::<Vec<EntityPath>>();

        let has_children = !children.is_empty();

        if content.is_none() && metadata.is_none() && !has_children && !directory_exists {
            return Ok(None);
        }

        // Determine the actual type
        let actual_type = if entity_type == "Auto" {
            let m = metadata.merged()?.ok_or_else(|| {
                anyhow!(
                    "Entity at '{:?}' has Auto type but no metadata",
                    entity_path.to_pathbuf(base_path)
                )
            })?;
            let t = m.get_str("type")?.ok_or_else(|| {
                anyhow!(
                    "Entity at '{:?}' has Auto type but metadata is missing 'type' key",
                    entity_path.to_pathbuf(base_path)
                )
            })?;
            if t == "Auto" {
                bail!(
                    "Entity at '{:?}' has metadata 'type' set to 'Auto', which is not allowed",
                    entity_path.to_pathbuf(base_path)
                );
            }
            t
        } else {
            if let Some(m) = metadata.merged()? {
                if let Some(t) = m.get_str("type")? {
                    if t != entity_type {
                        bail!("Entity at '{:?}' has type '{}' in metadata, but was expected to be '{}'", entity_path.to_pathbuf(base_path), t, entity_type);
                    }
                }
            }
            entity_type.to_string()
        };

        let entity_type_descriptor = self.get_entity_type(&actual_type)?;

        // Work through the children and load them.
        let mut loaded_children: Vec<Entity> = vec![];
        for child_entity_path in children {
            let child_name = child_entity_path.last_name().unwrap();
            if entity_type_descriptor.ignore.iter().any(|s| s == child_name) {
                continue;
            }
            let mut found_match = false;
            for child_rule in &entity_type_descriptor.children {
                let re = regex::Regex::new(&child_rule.name_regex).unwrap();
                if re.is_match(child_name) {
                    let child_entity = self.try_load_entity(
                        fs,
                        base_path,
                        &child_entity_path,
                        &child_rule.node_type,
                    ).with_context(|| format!("error loading child entity '{}' of type '{}' for parent entity '{:?}'", child_name, &child_rule.node_type, entity_path.to_pathbuf(base_path)))?;
                    if let Some(ce) = child_entity {
                        loaded_children.push(ce);
                        found_match = true;
                        break;
                    }
                }
            }
            if !found_match && !entity_type_descriptor.allow_additional {
                bail!(
                    "Unexpected child entity '{}' in entity '{:?}'",
                    child_name,
                    entity_path.to_pathbuf(base_path)
                );
            }
        }

        let entity = Entity {
            path: entity_path.clone(),
            node_type: actual_type,
            content,
            metadata,
            children: loaded_children,
        };
        Ok(Some(entity))
    }
}

impl Default for EntityLoader {
    fn default() -> Self {
        Self::new()
    }
}

pub struct EntityWriter {}

impl EntityWriter {
    pub fn write_entity(
        &self,
        fs: &mut dyn Xfs,
        base_path: &Path,
        entity: &Entity,
    ) -> anyhow::Result<()> {
        let entity_path = entity.path.to_pathbuf(base_path);

        let is_empty =
            entity.content.is_none() && entity.metadata.is_none() && entity.children.is_empty();
        let needs_directory = is_empty
            || entity.content.is_inside()
            || entity.metadata.source_at(MetaLocation::InsideSidecar).is_some()
            || entity
                .children
                .iter()
                .any(|c: &Entity| c.path.entries.last().unwrap().is_slash());
        if needs_directory {
            fs.create_dir_all(&entity_path)?;
        }

        let header_source = entity.metadata.source_at(MetaLocation::InHeader);

        if let Some(content) = entity.content.content() {
            let mut to_write = String::new();
            if let Some(source) = header_source {
                let MetaOrigin::Header { header_type, separator } = &source.origin else {
                    unreachable!("source_at(InHeader) returns a header origin")
                };
                match &source.state {
                    MetaState::Parsed(m) => to_write.push_str(&utils::format_metadata_header(
                        m,
                        *header_type,
                        separator.as_deref(),
                        content,
                    )?),
                    // A block we could not parse goes back exactly as it came, so a
                    // load/save cycle cannot destroy it (D3).
                    MetaState::Malformed { raw, .. } => to_write.push_str(raw),
                }
            }
            to_write.push_str(content);

            let path = match &entity.content {
                EntityContent::Parallel(_) => entity_path.with_added_extension("md"),
                EntityContent::Inside(_) => entity_path.join("content.md"),
                _ => unreachable!(),
            };
            fs.writer(&path)?.write_all(to_write.as_bytes())?;
        } else if header_source.is_some() {
            bail!(
                "Metadata from header requires content for entity at {:?}",
                entity.path.to_pathbuf(base_path)
            );
        }

        // Every sidecar source is written, so a node carrying more than one round-trips.
        for source in entity.metadata.sources() {
            let path = match source.location() {
                MetaLocation::InHeader => continue,
                // TODO(Task 7): route through placement, which appends rather than
                // substitutes. `with_extension` here is C1.
                MetaLocation::ParallelSidecar => entity_path.with_extension("meta.toml"),
                MetaLocation::InsideSidecar => entity_path.join("meta.toml"),
            };
            let text = match &source.state {
                MetaState::Parsed(m) => toml::to_string(&m.value)?,
                MetaState::Malformed { raw, .. } => raw.clone(),
            };
            fs.writer(&path)?.write_all(text.as_bytes())?;
        }

        for child in &entity.children {
            self.write_entity(fs, base_path, child)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum EntityContent {
    None,
    /// content is found at entity.md
    Parallel(String),
    /// content is found at entity/content.md
    Inside(String),
}

impl EntityContent {
    pub fn is_none(&self) -> bool {
        matches!(self, EntityContent::None)
    }

    pub fn content(&self) -> Option<&str> {
        match self {
            EntityContent::None => None,
            EntityContent::Parallel(c) => Some(c.as_str()),
            EntityContent::Inside(c) => Some(c.as_str()),
        }
    }

    pub fn parallel<S: Into<String>>(s: S) -> EntityContent {
        EntityContent::Parallel(s.into())
    }

    pub fn inside<S: Into<String>>(s: S) -> EntityContent {
        EntityContent::Inside(s.into())
    }

    fn is_inside(&self) -> bool {
        matches!(self, EntityContent::Inside(_))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HeaderType {
    Toml,
    Yaml,
}

/// The parse state of one metadata source.
///
/// A source that failed to parse is kept rather than dropped, with its raw text and the
/// error, so a caller can inspect and repair it and so a save cannot destroy a file the
/// library could not read (D3).
#[derive(Debug, PartialEq, Clone)]
pub enum MetaState {
    Parsed(Metadata),
    Malformed { raw: String, error: String },
}

/// Where one metadata source sits — and, for front matter, how it was written, so a save
/// reproduces the block rather than normalising it.
///
/// This is the *observed* counterpart to [`MetaLocation`], which stays payload-free
/// because intent and per-key routing are expressed in it.
#[derive(Debug, PartialEq, Clone)]
pub enum MetaOrigin {
    /// Front matter in the content file, wherever layout put that file.
    Header {
        header_type: HeaderType,
        separator: Option<String>,
    },
    /// `S.meta.toml`
    ParallelSidecar,
    /// `S/meta.toml`
    InsideSidecar,
}

impl MetaOrigin {
    /// The layout-facing location of this origin.
    pub fn location(&self) -> MetaLocation {
        match self {
            MetaOrigin::Header { .. } => MetaLocation::InHeader,
            MetaOrigin::ParallelSidecar => MetaLocation::ParallelSidecar,
            MetaOrigin::InsideSidecar => MetaLocation::InsideSidecar,
        }
    }
}

/// One metadata source, as observed on disk.
#[derive(Debug, PartialEq, Clone)]
pub struct MetaSource {
    pub origin: MetaOrigin,
    pub state: MetaState,
}

impl MetaSource {
    pub fn location(&self) -> MetaLocation {
        self.origin.location()
    }

    /// The parsed table, or `None` if this source is malformed.
    pub fn metadata(&self) -> Option<&Metadata> {
        match &self.state {
            MetaState::Parsed(m) => Some(m),
            MetaState::Malformed { .. } => None,
        }
    }

    /// The text as found on disk, for a malformed source only.
    pub fn raw(&self) -> Option<&str> {
        match &self.state {
            MetaState::Malformed { raw, .. } => Some(raw),
            MetaState::Parsed(_) => None,
        }
    }

    /// The parse error, for a malformed source only.
    pub fn error(&self) -> Option<&str> {
        match &self.state {
            MetaState::Malformed { error, .. } => Some(error),
            MetaState::Parsed(_) => None,
        }
    }

    pub fn is_malformed(&self) -> bool {
        matches!(self.state, MetaState::Malformed { .. })
    }
}

/// One key held by two or more sources with differing values — §4.5's unroutable case.
#[derive(Debug, PartialEq, Clone)]
pub struct MetaConflict {
    pub key: String,
    pub locations: Vec<MetaLocation>,
}

/// An entity's metadata, as the set of sources observed on disk.
///
/// Zero sources means no metadata; one is the ordinary case; several merge per key
/// (§4.5), which is observable but never *intendable* — a schema declares one layout and
/// so one sidecar location.
///
/// Sources are held in [`MetaLocation`] order, and every method here is a function of
/// that ordered list. Two `EntityMeta` that compare equal therefore also behave alike —
/// merge the same way, and route writes to the same files.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct EntityMeta {
    sources: Vec<MetaSource>,
}

impl EntityMeta {
    /// Builds from observed sources, ordering them by location so that merge precedence
    /// does not depend on the order the loader happened to probe in.
    pub fn of(mut sources: Vec<MetaSource>) -> EntityMeta {
        sources.sort_by_key(|s| s.location());
        EntityMeta { sources }
    }

    /// A single parsed source at `origin`.
    pub fn at(origin: MetaOrigin, m: Metadata) -> EntityMeta {
        EntityMeta::of(vec![MetaSource { origin, state: MetaState::Parsed(m) }])
    }

    pub fn parallel(m: Metadata) -> EntityMeta {
        EntityMeta::at(MetaOrigin::ParallelSidecar, m)
    }

    pub fn inside(m: Metadata) -> EntityMeta {
        EntityMeta::at(MetaOrigin::InsideSidecar, m)
    }

    pub fn in_header(m: Metadata, separator: Option<String>, header_type: HeaderType) -> EntityMeta {
        EntityMeta::at(MetaOrigin::Header { header_type, separator }, m)
    }

    pub fn sources(&self) -> &[MetaSource] {
        &self.sources
    }

    pub fn is_none(&self) -> bool {
        self.sources.is_empty()
    }

    /// The single source, when there is exactly one — the ordinary case.
    pub fn single(&self) -> Option<&MetaSource> {
        match self.sources.as_slice() {
            [only] => Some(only),
            _ => None,
        }
    }

    /// The single source's parsed table, mutably — `None` unless there is exactly one
    /// source and it parsed.
    pub fn single_parsed_mut(&mut self) -> Option<&mut Metadata> {
        match self.sources.as_mut_slice() {
            [only] => match &mut only.state {
                MetaState::Parsed(m) => Some(m),
                MetaState::Malformed { .. } => None,
            },
            _ => None,
        }
    }

    /// The source at `location`, if present.
    pub fn source_at(&self, location: MetaLocation) -> Option<&MetaSource> {
        self.sources.iter().find(|s| s.location() == location)
    }

    pub fn locations(&self) -> Vec<MetaLocation> {
        self.sources.iter().map(|s| s.location()).collect()
    }

    pub fn malformed(&self) -> Vec<&MetaSource> {
        self.sources.iter().filter(|s| s.is_malformed()).collect()
    }

    /// Which source holds `key`. When several do, the one that wins the merge.
    pub fn location_of(&self, key: &str) -> Option<MetaLocation> {
        self.sources
            .iter()
            .rev()
            .find(|s| s.metadata().and_then(|m| m.value.get(key)).is_some())
            .map(|s| s.location())
    }

    /// Where a write of `key` should land. §4.5.
    ///
    /// A key that already exists never moves — the source holding it wins. A key new to
    /// this node goes to its single source when it has exactly one (§4.3 step 2), and
    /// otherwise to `intended`, the location this node's layout would have chosen.
    ///
    /// Depends only on which locations are present, never on the order they were
    /// observed in.
    pub fn write_location(&self, key: &str, intended: MetaLocation) -> MetaLocation {
        if let Some(location) = self.location_of(key) {
            return location;
        }
        match self.single() {
            Some(only) => only.location(),
            None => intended,
        }
    }

    /// Keys held by several sources with differing values, each listing every source it
    /// was found in.
    pub fn conflicts(&self) -> Vec<MetaConflict> {
        let mut seen: std::collections::BTreeMap<&str, Vec<(MetaLocation, &toml::Value)>> =
            Default::default();
        for source in &self.sources {
            let Some(table) = source.metadata().and_then(|m| m.value.as_table()) else {
                continue;
            };
            for (key, value) in table {
                seen.entry(key).or_default().push((source.location(), value));
            }
        }
        seen.into_iter()
            .filter(|(_, found)| found.iter().any(|(_, v)| *v != found[0].1))
            .map(|(key, found)| MetaConflict {
                key: key.to_string(),
                locations: found.into_iter().map(|(l, _)| l).collect(),
            })
            .collect()
    }

    /// Every parsed source merged into one table, later locations winning.
    /// `Ok(None)` when there is nothing parsed to merge.
    ///
    /// # Errors
    ///
    /// Returns an error if a source parsed to something other than a table.
    pub fn merged(&self) -> anyhow::Result<Option<Metadata>> {
        let mut out = toml::Table::new();
        let mut any = false;
        for source in &self.sources {
            let Some(m) = source.metadata() else { continue };
            let table = m.value.as_table().ok_or_else(|| {
                anyhow!("Metadata at {:?} is not a table", source.location())
            })?;
            for (key, value) in table {
                out.insert(key.clone(), value.clone());
            }
            any = true;
        }
        Ok(any.then(|| Metadata { value: toml::Value::Table(out) }))
    }

    pub fn get_str(&self, key: &str) -> anyhow::Result<Option<String>> {
        match self.merged()? {
            Some(m) => m.get_str(key),
            None => Ok(None),
        }
    }

    pub fn get_vec_of_string(&self, key: &str) -> anyhow::Result<Option<Vec<String>>> {
        match self.merged()? {
            Some(m) => m.get_vec_of_string(key),
            None => Ok(None),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Entity {
    pub path: EntityPath,
    pub node_type: String,
    pub content: EntityContent,
    pub metadata: EntityMeta,
    pub children: Vec<Entity>,
}

/// Parse front matter (TOML or YAML) from a Markdown content string.
///
/// Returns `(metadata, separator, body_content, header_type)` or `None` if no
/// valid front matter block is found at the start of the content.
pub fn parse_front_matter(
    content: &str,
) -> Option<(Metadata, Option<String>, String, HeaderType)> {
    utils::parse_header(content)
}

/// Serialize front matter (TOML or YAML) back into a Markdown content string,
/// the inverse of `parse_front_matter`.
pub fn format_metadata_header(
    metadata: &Metadata,
    header_type: HeaderType,
    sep: Option<&str>,
    content_body: &str,
) -> anyhow::Result<String> {
    utils::format_metadata_header(metadata, header_type, sep, content_body)
}

#[cfg(test)]
mod common {
    use super::*;
    use crate::placement::Edge;
    use crate::schema::ChildEntityRules;

    pub fn dummy_loader() -> EntityLoader {
        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "TestType".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^child[0-9]*$".to_string(),
                    node_type: "ChildTestType".to_string(),
                    required: false,
                    edge: Edge::Slash,
                    multiple: true,
                }],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "ChildTestType".to_string(),
                children: vec![],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();
        loader
    }
}

#[cfg(test)]
mod meta_tests {
    use super::*;
    use crate::placement::MetaLocation;

    /// Builds a source the way the loader will: parse the text, and on failure keep it
    /// verbatim alongside the real parse error.
    fn source(origin: MetaOrigin, raw: &str) -> MetaSource {
        let state = match toml::from_str::<toml::Value>(raw) {
            Ok(value) => MetaState::Parsed(Metadata { value }),
            Err(e) => MetaState::Malformed { raw: raw.to_string(), error: e.to_string() },
        };
        MetaSource { origin, state }
    }

    fn header(raw: &str) -> MetaSource {
        source(MetaOrigin::Header { header_type: HeaderType::Toml, separator: None }, raw)
    }

    fn parallel(raw: &str) -> MetaSource {
        source(MetaOrigin::ParallelSidecar, raw)
    }

    /// §4.5: an entity with no metadata anywhere.
    #[test]
    fn no_sources_is_no_metadata() {
        let m = EntityMeta::default();
        assert!(m.is_none());
        assert!(m.merged().unwrap().is_none());
        assert_eq!(m.get_str("type").unwrap(), None);
    }

    /// §4.5: the ordinary case — one source, read straight through.
    #[test]
    fn a_single_source_reads_straight_through() {
        let m = EntityMeta::of(vec![parallel("type = \"T\"")]);
        assert!(m.single().is_some());
        assert_eq!(m.get_str("type").unwrap().as_deref(), Some("T"));
        assert_eq!(m.location_of("type"), Some(MetaLocation::ParallelSidecar));
        assert!(m.conflicts().is_empty());
    }

    /// A front-matter source remembers how it was written, so a save reproduces the
    /// block byte for byte rather than normalising YAML to TOML. §4.5.
    #[test]
    fn an_in_header_source_remembers_its_front_matter_shape() {
        let m = EntityMeta::of(vec![source(
            MetaOrigin::Header {
                header_type: HeaderType::Yaml,
                separator: Some("\n".to_string()),
            },
            "type = \"T\"",
        )]);
        match &m.single().unwrap().origin {
            MetaOrigin::Header { header_type, separator } => {
                assert_eq!(*header_type, HeaderType::Yaml);
                assert_eq!(separator.as_deref(), Some("\n"));
            }
            other => panic!("expected a header origin, got {:?}", other),
        }
        // A sidecar has no such shape to remember — the type makes that unrepresentable.
        assert_eq!(m.locations(), vec![MetaLocation::InHeader]);
    }

    /// D2: keys from different sources merge, and each key remembers which source holds
    /// it — which is what lets §4.5 write an update back to the file it came from.
    #[test]
    fn disjoint_keys_merge_and_each_remembers_its_source() {
        let m = EntityMeta::of(vec![header("type = \"T\""), parallel("word_count = 12")]);

        let merged = m.merged().unwrap().unwrap();
        assert_eq!(merged.get_str("type").unwrap().as_deref(), Some("T"));
        assert_eq!(merged.value.get("word_count").and_then(|v| v.as_integer()), Some(12));

        for (key, expected) in [
            ("type", Some(MetaLocation::InHeader)),
            ("word_count", Some(MetaLocation::ParallelSidecar)),
            ("absent", None),
        ] {
            assert_eq!(m.location_of(key), expected, "location of {:?}", key);
        }
        assert!(m.conflicts().is_empty());
    }

    /// D2: two sources holding the same key agree or they don't. Only disagreement is a
    /// conflict — the default policy makes that an error (§9.1).
    #[test]
    fn a_shared_key_conflicts_only_when_the_values_differ() {
        for (in_header, in_sidecar, expected) in [
            ("type = \"T\"", "type = \"T\"", vec![]),
            ("type = \"A\"", "type = \"B\"", vec!["type"]),
            ("type = \"A\"\nn = 1", "type = \"A\"\nn = 2", vec!["n"]),
        ] {
            let m = EntityMeta::of(vec![header(in_header), parallel(in_sidecar)]);
            let conflicts = m.conflicts();
            let keys: Vec<&str> = conflicts.iter().map(|c| c.key.as_str()).collect();
            assert_eq!(keys, expected, "{:?} against {:?}", in_header, in_sidecar);
        }
    }

    /// §4.5: a conflict names every source holding the key, so a caller can repair it.
    /// The merge still resolves, by `MetaLocation` order — reachable only when the
    /// policy downgrades the finding below `Error`.
    #[test]
    fn a_conflict_names_its_sources_and_resolves_by_location_order() {
        let m = EntityMeta::of(vec![header("type = \"A\""), parallel("type = \"B\"")]);
        let conflicts = m.conflicts();
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].key, "type");
        assert_eq!(
            conflicts[0].locations,
            vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar]
        );
        let merged = m.merged().unwrap().unwrap();
        assert_eq!(merged.get_str("type").unwrap().as_deref(), Some("B"), "later location wins");
    }

    /// Merge precedence is a property of the locations, not of the order the loader
    /// happened to probe them in.
    #[test]
    fn construction_order_does_not_change_the_result() {
        assert_eq!(
            EntityMeta::of(vec![header("type = \"A\""), parallel("type = \"B\"")]),
            EntityMeta::of(vec![parallel("type = \"B\""), header("type = \"A\"")])
        );
    }

    /// §4.5: where a write lands. A key that already exists never moves; a key new to
    /// this node goes to its only source, or to the layout's intent when there is a
    /// choice to be made. Each row is asserted in both source orders, because two
    /// `EntityMeta` that compare equal must also behave alike.
    #[test]
    fn a_write_lands_on_the_source_owning_the_key_or_else_on_intent() {
        let intended = MetaLocation::InsideSidecar;
        for (sources, key, expected) in [
            (vec![], "anything", MetaLocation::InsideSidecar),
            // One established source takes new keys too — §4.3 step 2.
            (vec![parallel("n = 1")], "n", MetaLocation::ParallelSidecar),
            (vec![parallel("n = 1")], "new", MetaLocation::ParallelSidecar),
            // With two, an existing key stays put...
            (vec![header("t = \"T\""), parallel("n = 1")], "t", MetaLocation::InHeader),
            (vec![header("t = \"T\""), parallel("n = 1")], "n", MetaLocation::ParallelSidecar),
            // ...and only a genuinely new key falls through to intent.
            (vec![header("t = \"T\""), parallel("n = 1")], "new", MetaLocation::InsideSidecar),
        ] {
            let reversed: Vec<MetaSource> = sources.iter().rev().cloned().collect();
            let label = format!("{:?} in {} sources", key, sources.len());
            assert_eq!(
                EntityMeta::of(sources).write_location(key, intended),
                expected,
                "{}", label
            );
            assert_eq!(
                EntityMeta::of(reversed).write_location(key, intended),
                expected,
                "{}, reversed", label
            );
        }
    }

    /// D3 / C10: a source that does not parse keeps its raw text and its error, and does
    /// not take the other source's keys down with it. Previously the parse error
    /// propagated and the whole entity failed to load.
    #[test]
    fn a_malformed_source_is_retained_beside_a_good_one() {
        let m = EntityMeta::of(vec![
            parallel("this is not = = toml"),
            header("type = \"T\""),
        ]);
        assert!(!m.is_none(), "a file that exists but does not parse is still metadata");

        let bad = m.malformed();
        assert_eq!(bad.len(), 1);
        assert_eq!(bad[0].location(), MetaLocation::ParallelSidecar);
        assert_eq!(bad[0].raw(), Some("this is not = = toml"), "kept verbatim for repair");
        assert!(bad[0].error().is_some(), "and the parse error with it");

        assert_eq!(m.get_str("type").unwrap().as_deref(), Some("T"), "the good source still reads");
    }
}

#[cfg(test)]
mod entity_tests {

    use inscenerator_xfs::mockfs;
    use crate::placement::Edge;
    use crate::schema::ChildEntityRules;

    use super::common::*;
    use super::*;

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

    // TODO: : Should we support empty entities?
    // #[test]
    // fn test_load_entity_empty() {
    //     let mut fs = mockfs::MockFS::new();
    //     fs.create_dir_all(&PathBuf::from("foo/entity1")).unwrap();
    //     let fs = fs;

    //     let loader = dummy_loader();

    //     let entity_path = EntityPath::empty().extend_slash("entity1");
    //     let entity = loader
    //         .try_load_entity(
    //             &fs,
    //             &PathBuf::from("foo"),
    //             &entity_path,
    //             "TestType",
    //         )
    //         .unwrap();
    //     let e = entity.unwrap();
    //     assert!(e.content.is_none());
    //     assert_eq!(e.path, entity_path);
    //     assert!(e.metadata.is_none());
    //     assert!(e.children.is_empty());
    //     assert_eq!(e.node_type, "TestType".to_string());
    // }

    #[test]
    fn test_load_entity_with_inside_content_only() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Hello, world!");
        let fs = fs;

        let loader = dummy_loader();

        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.expect("Entity should be loaded");
        assert_eq!(e.content, EntityContent::inside("Hello, world!"));
        assert_eq!(e.path, entity_path);
        assert!(e.metadata.is_none());
        assert!(e.children.is_empty());
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_with_parallel_content_only() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", "Hello, world!");
        let fs = fs;

        let loader = dummy_loader();

        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.expect("Entity should be loaded");
        assert_eq!(e.content, EntityContent::parallel("Hello, world!"));
        assert_eq!(e.path, entity_path);
        assert!(e.metadata.is_none());
        assert!(e.children.is_empty());
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_with_inside_metadata() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "meta.toml", "bar=\"foo\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.content, EntityContent::None);
        assert!(e.children.is_empty());
        assert_eq!(e.path, entity_path);
        assert_eq!(
            e.metadata,
            EntityMeta::inside(Metadata {
                value: toml::from_str("bar=\"foo\"\n").unwrap()
            })
        );
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_with_parallel_metadata() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "bar=\"foo\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.content, EntityContent::None);
        assert!(e.children.is_empty());
        assert_eq!(e.path, entity_path);
        assert_eq!(
            e.metadata,
            EntityMeta::parallel(Metadata {
                value: toml::from_str("bar=\"foo\"\n").unwrap()
            })
        );
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_with_slash_child_with_inside_content() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1/child1", "content.md", "Child content");
        let fs = fs;
        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.children.len(), 1);
        let child = &e.children[0];
        assert_eq!(child.path, entity_path.extend_slash("child1"));
        assert_eq!(child.content, EntityContent::inside("Child content"));
        assert_eq!(child.node_type, "ChildTestType");
    }

    #[test]
    fn test_load_entity_with_slash_child_with_parallel_content() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "child1.md", "Child content");
        let fs = fs;
        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.children.len(), 1);
        let child = &e.children[0];
        assert_eq!(child.path, entity_path.extend_slash("child1"));
        assert_eq!(child.content, EntityContent::parallel("Child content"));
        assert_eq!(child.node_type, "ChildTestType");
    }

    #[test]
    fn test_load_entity_with_two_dot_children() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.child1.md", "Child 1 content");
        create_file_with_content(&mut fs, "foo", "entity1.child2.md", "Child 2 content");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.children.len(), 2);
        let child1 = &e.children[0];
        assert_eq!(child1.path, entity_path.extend_dot("child1"));
        assert_eq!(child1.content.content().unwrap(), "Child 1 content");
        assert_eq!(child1.children.len(), 0);
        assert_eq!(child1.node_type, "ChildTestType");
        let child2 = &e.children[1];
        assert_eq!(child2.path, entity_path.extend_dot("child2"));
        assert_eq!(child2.content.content().unwrap(), "Child 2 content");
        assert_eq!(child2.node_type, "ChildTestType");
        assert_eq!(child2.children.len(), 0);
    }

    #[test]
    pub fn can_load_nested_entry_value() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.child1.md", "Child 1 content");
        let fs = fs;
        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(
                &fs,
                &PathBuf::from("foo"),
                &entity_path.extend_dot("child1"),
                "ChildTestType",
            )
            .unwrap();
        let child1 = entity.unwrap();
        assert_eq!(child1.path, entity_path.extend_dot("child1"));
        assert_eq!(child1.content.content().unwrap(), "Child 1 content");
        assert_eq!(child1.node_type, "ChildTestType");
        assert_eq!(child1.children.len(), 0);
    }

    #[test]
    fn test_find_children_nested_dot() {
        // file system:
        // foo/
        //   entity1.child1.md
        //   entity1.child1.meta.toml
        //   entity1.child2/
        //     content.md
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.child1.md", "Child 1 content");
        create_file_with_content(&mut fs, "foo", "entity1.child1.meta.toml", "bar=\"foo\"\n");
        create_file_with_content(
            &mut fs,
            "foo/entity1.child2",
            "content.md",
            "Child 2 content",
        );
        let fs = fs;

        let children = utils::find_dot_children(
            &fs,
            &PathBuf::from("foo"),
            &EntityPath::empty().extend_slash("entity1"),
        )
        .unwrap();
        assert_eq!(
            children,
            vec![
                EntityPath::empty()
                    .extend_slash("entity1")
                    .extend_dot("child1"),
                EntityPath::empty()
                    .extend_slash("entity1")
                    .extend_dot("child2"),
            ]
        );
    }

    #[test]
    fn test_find_children_nested_slash() {
        // file system:
        // foo/
        //   entity1/
        //      child1.md
        //      child1.meta.toml
        //      child2/
        //         content.md
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "child1.md", "Child 1 content");
        create_file_with_content(&mut fs, "foo/entity1", "child1.meta.toml", "bar=\"foo\"\n");
        create_file_with_content(
            &mut fs,
            "foo/entity1/child2",
            "content.md",
            "Child 2 content",
        );
        let fs = fs;

        let children = utils::find_slash_children(
            &fs,
            &PathBuf::from("foo"),
            &EntityPath::empty().extend_slash("entity1"),
        )
        .unwrap();
        assert_eq!(
            children,
            vec![
                EntityPath::empty()
                    .extend_slash("entity1")
                    .extend_slash("child1"),
                EntityPath::empty()
                    .extend_slash("entity1")
                    .extend_slash("child2"),
            ]
        );
    }

    #[test]
    fn loads_root_entity() {
        // file system looks like this:
        //
        // foo/
        //   content.md
        //   child.md
        // foo.md  (should be ignored
        // foo.bar/
        //     content.md  (should be ignored)
        //
        let entity_path = EntityPath::empty();
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "content.md", "Root content");
        create_file_with_content(&mut fs, "foo", "child.md", "Child content");
        create_file_with_content(&mut fs, "", "foo.md", "Should be ignored");
        create_file_with_content(&mut fs, "foo.bar", "content.md", "Should be ignored");
        let fs = fs;

        let loader = dummy_loader();
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();

        let e = entity.unwrap();
        assert_eq!(e.content, EntityContent::inside("Root content"));
        assert_eq!(e.children.len(), 1);
        let child = &e.children[0];
        assert_eq!(child.path, entity_path.extend_slash("child"));
        assert_eq!(child.content, EntityContent::parallel("Child content"));
        assert_eq!(child.node_type, "ChildTestType");
    }

    #[test]
    pub fn simple_project() {
        let mut fs = mockfs::MockFS::new();
        // project
        //   ├── 010_chapter1
        //   │   ├── 010_scene1.md
        //   │   └── 020_scene2.md
        //   └── notes
        //        └── a_note.md

        create_file_with_content(
            &mut fs,
            "project/010_chapter1",
            "010_scene1.md",
            "Scene 1 content",
        );
        create_file_with_content(
            &mut fs,
            "project/010_chapter1",
            "020_scene2.md",
            "Scene 2 content",
        );
        create_file_with_content(&mut fs, "project/notes", "a_note.md", "A note content");

        let mut loader = EntityLoader::new();

        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "Project".to_string(),
                children: vec![
                    ChildEntityRules {
                        name_regex: "^[0-9]+_".to_string(),
                        node_type: "Chapter".to_string(),
                        required: false,
                        edge: Edge::Slash,
                        multiple: true,
                    },
                    ChildEntityRules {
                        name_regex: "^notes$".to_string(),
                        node_type: "Notes".to_string(),
                        required: false,
                        edge: Edge::Slash,
                        multiple: true,
                    },
                ],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "Chapter".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^[0-9]+_".to_string(),
                    node_type: "Scene".to_string(),
                    required: false,
                    edge: Edge::Slash,
                    multiple: true,
                }],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "Scene".to_string(),
                children: vec![],
                allow_additional: true,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "Notes".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: ".*".to_string(),
                    node_type: "Notes".to_string(),
                    required: false,
                    edge: Edge::Slash,
                    multiple: true,
                }],
                allow_additional: true,
                layout: None,
                ignore: vec![],
            },
        ).unwrap();

        let entity_path = EntityPath::empty();
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &entity_path, "Project")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.children.len(), 2);
        let chapter = &e.children[0];
        assert_eq!(chapter.path, entity_path.extend_slash("010_chapter1"));
        assert_eq!(chapter.node_type, "Chapter");
        assert_eq!(chapter.children.len(), 2);
        let scene1 = &chapter.children[0];
        assert_eq!(
            scene1.path,
            entity_path
                .extend_slash("010_chapter1")
                .extend_slash("010_scene1")
        );
        assert_eq!(scene1.content, EntityContent::parallel("Scene 1 content"));
        let scene2 = &chapter.children[1];
        assert_eq!(
            scene2.path,
            entity_path
                .extend_slash("010_chapter1")
                .extend_slash("020_scene2")
        );
        assert_eq!(scene2.content, EntityContent::parallel("Scene 2 content"));
        let notes = &e.children[1];
        assert_eq!(notes.path, entity_path.extend_slash("notes"));
        assert_eq!(notes.node_type, "Notes");
        assert_eq!(notes.children.len(), 1);
        let note = &notes.children[0];
        assert_eq!(
            note.path,
            entity_path.extend_slash("notes").extend_slash("a_note")
        );
        assert_eq!(note.content, EntityContent::parallel("A note content"));
    }

    // #[test]
    // fn test_write_empty_entity() {
    //     let entity_path = EntityPath::empty().extend_slash("an_entity");
    //     let entity = Entity {
    //         content: None,
    //         children: vec![],
    //         path: entity_path,
    //         metadata: None,
    //         node_type: String::from("TestType"),
    //     };
    //     let writer = EntityWriter {};
    //     let mut fs = mockfs::MockFS::new();
    //     fs.create_dir(&PathBuf::from("foo")).unwrap();
    //     writer
    //         .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
    //         .unwrap();
    //     // Now check that the directory exists and is empty.
    //     let de = fs.resolve_path(&PathBuf::from("foo/an_entity")).unwrap();
    //     let de = de.as_dir().unwrap();
    //     assert!(de.entries.is_empty());
    // }

    #[test]
    fn test_write_entity_with_content_inside() {
        let content = "Hello, world!";
        let entity_path = EntityPath::empty().extend_slash("an_entity");

        let entity = Entity {
            content: EntityContent::inside(content.to_string()),
            children: vec![],
            path: entity_path,
            metadata: EntityMeta::default(),
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        // Now check that the directory exists and has the content file.
        let de = fs.resolve_path(&PathBuf::from("foo/an_entity")).unwrap();
        let de = de.as_dir().unwrap();
        assert_eq!(de.num_entries(), 1);
        let file_content = fs.get_str(&PathBuf::from("foo/an_entity/content.md")).unwrap();
        assert_eq!(file_content, content);
    }

    #[test]
    fn test_write_entity_with_content_parallel() {
        let content = "Hello, world!";
        let entity_path = EntityPath::empty().extend_slash("an_entity");

        let entity = Entity {
            content: EntityContent::parallel(content.to_string()),
            children: vec![],
            path: entity_path,
            metadata: EntityMeta::default(),
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        // Now check that the content file.
        let file_content = fs.get_str(&PathBuf::from("foo/an_entity.md")).unwrap();
        assert_eq!(file_content, content);
    }

    #[test]
    fn test_write_entity_with_metadata() {
        let metadata = Metadata {
            value: toml::from_str("bar = \"foo\"\n").unwrap(),
        };
        let entity_path = EntityPath::empty().extend_slash("an_entity");

        let entity = Entity {
            content: EntityContent::None,
            children: vec![],
            path: entity_path,
            metadata: EntityMeta::inside(metadata),
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        // Now check that the directory exists and has the metadata file.
        let de = fs.resolve_path(&PathBuf::from("foo/an_entity")).unwrap();
        let de = de.as_dir().unwrap();
        assert_eq!(de.num_entries(), 1);
        let file_metadata = fs.get_str(&PathBuf::from("foo/an_entity/meta.toml")).unwrap();
        assert_eq!(file_metadata, "bar = \"foo\"\n");
    }

    #[test]
    fn test_write_entity_with_children_of_directory_type() {
        // Final structure should look like
        // an_entity/
        //   child1/
        //       content.md
        //   child2/
        //       content.md
        let entity_path = EntityPath::empty().extend_slash("an_entity");

        let child1 = Entity {
            content: EntityContent::inside("Child 1 content".to_string()),
            children: vec![],
            path: entity_path.extend_slash("child1"),
            metadata: EntityMeta::default(),
            node_type: String::from("ChildTestType"),
        };
        let child2 = Entity {
            content: EntityContent::inside("Child 2 content".to_string()),
            children: vec![],
            path: entity_path.extend_slash("child2"),
            metadata: EntityMeta::default(),
            node_type: String::from("ChildTestType"),
        };
        let entity = Entity {
            content: EntityContent::None,
            children: vec![child1, child2],
            path: entity_path,
            metadata: EntityMeta::default(),
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        // Now check that the directory exists and has the child directories.
        let de = fs.resolve_path(&PathBuf::from("foo/an_entity")).unwrap();
        let de = de.as_dir().unwrap();
        assert_eq!(de.num_entries(), 2);
        let child1_de = fs
            .resolve_path(&PathBuf::from("foo/an_entity/child1"))
            .unwrap();
        let child1_de = child1_de.as_dir().unwrap();
        assert_eq!(child1_de.num_entries(), 1);
        let child1_content = fs.get_str(&PathBuf::from("foo/an_entity/child1/content.md")).unwrap();
        assert_eq!(child1_content, "Child 1 content");
        let child2_de = fs
            .resolve_path(&PathBuf::from("foo/an_entity/child2"))
            .unwrap();
        let child2_de = child2_de.as_dir().unwrap();
        assert_eq!(child2_de.num_entries(), 1);
        let child2_content = fs.get_str(&PathBuf::from("foo/an_entity/child2/content.md")).unwrap();
        assert_eq!(child2_content, "Child 2 content");
    }

    #[test]
    fn test_write_entity_with_children_of_file_type() {
        // Final structure should look like
        // an_entity/
        //   an_entity.child1.md
        //   an_entity.child2.md
        let entity_path = EntityPath::empty().extend_slash("an_entity");

        let child1 = Entity {
            content: EntityContent::parallel("Child 1 content".to_string()),
            children: vec![],
            path: entity_path.extend_dot("child1"),
            metadata: EntityMeta::default(),
            node_type: String::from("ChildTestType"),
        };
        let child2 = Entity {
            content: EntityContent::parallel("Child 2 content".to_string()),
            children: vec![],
            path: entity_path.extend_dot("child2"),
            metadata: EntityMeta::default(),
            node_type: String::from("ChildTestType"),
        };
        let entity = Entity {
            content: EntityContent::None,
            children: vec![child1, child2],
            path: entity_path,
            metadata: EntityMeta::default(),
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        let child1_content = fs.get_str(&PathBuf::from("foo/an_entity.child1.md")).unwrap();
        assert_eq!(child1_content, "Child 1 content");
        let child2_content = fs.get_str(&PathBuf::from("foo/an_entity.child2.md")).unwrap();
        assert_eq!(child2_content, "Child 2 content");
    }

    fn load_entity(fs: &dyn Xfs, base: &str, name: &str) -> Entity {
        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash(name);
        loader
            .try_load_entity(fs, &PathBuf::from(base), &entity_path, "TestType")
            .unwrap()
            .expect("Entity should be loaded")
    }

    fn setup_and_load(content: &str) -> (Entity, mockfs::MockFS) {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", content);
        (load_entity(&fs, "foo", "entity1"), fs)
    }

    /// Destructures the in-header source, failing with what was actually there.
    fn header_source(meta: &EntityMeta) -> (&Metadata, Option<&str>, HeaderType) {
        let Some(source) = meta.source_at(MetaLocation::InHeader) else {
            panic!("Expected InHeader metadata, got {:?}", meta);
        };
        let MetaOrigin::Header { header_type, separator } = &source.origin else {
            unreachable!("source_at(InHeader) returned a non-header origin")
        };
        (
            source.metadata().expect("header metadata parsed"),
            separator.as_deref(),
            *header_type,
        )
    }

    fn check_header_meta(
        meta: &EntityMeta,
        key: &str,
        expected_val: &str,
        expected_sep: Option<&str>,
    ) {
        let (m, sep, _) = header_source(meta);
        assert_eq!(m.value.get(key).unwrap().as_str().unwrap(), expected_val);
        assert_eq!(sep, expected_sep);
    }

    #[test]
    fn test_load_entity_with_header_no_separator() {
        let content = "```toml\nfoo = \"bar\"\n```\n\nActual content";
        let (e, _) = setup_and_load(content);
        assert_eq!(e.content, EntityContent::parallel("\nActual content"));
        check_header_meta(&e.metadata, "foo", "bar", None);
    }

    #[test]
    fn test_load_entity_with_header_and_separator() {
        let content = "```toml\nfoo = \"bar\"\n```\n\n---\n\nActual content";
        let (e, _) = setup_and_load(content);
        assert_eq!(e.content, EntityContent::parallel("\nActual content"));
        check_header_meta(&e.metadata, "foo", "bar", Some("\n---\n"));
    }

    /// D2: a header beside a sidecar is no longer refused — both sources load and merge.
    #[test]
    fn test_load_entity_merges_header_and_meta_toml() {
        let content = "```toml\nfoo = \"bar\"\n```\nActual content";
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", content);
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "other = \"meta\"\n");

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap()
            .unwrap();

        assert_eq!(
            e.metadata.locations(),
            vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar]
        );
        assert_eq!(e.metadata.get_str("foo").unwrap().as_deref(), Some("bar"));
        assert_eq!(e.metadata.get_str("other").unwrap().as_deref(), Some("meta"));
        assert!(e.metadata.conflicts().is_empty());
    }

    #[test]
    fn test_load_entity_with_yaml_header() {
        let content = "---\nfoo: bar\n---\n\nActual content";
        let (e, _) = setup_and_load(content);
        assert_eq!(e.content, EntityContent::parallel("\nActual content"));
        let (m, sep, header_type) = header_source(&e.metadata);
        assert_eq!(m.value.get("foo").unwrap().as_str().unwrap(), "bar");
        assert_eq!(sep, None);
        assert_eq!(header_type, HeaderType::Yaml);
    }

    #[test]
    fn test_load_entity_with_yaml_header_trailing_spaces() {
        let content = "---  \nfoo: bar\n--- \t\n\nActual content";
        let (e, _) = setup_and_load(content);
        assert_eq!(e.content, EntityContent::parallel("\nActual content"));
        let (m, _, header_type) = header_source(&e.metadata);
        assert_eq!(m.value.get("foo").unwrap().as_str().unwrap(), "bar");
        assert_eq!(header_type, HeaderType::Yaml);
    }

    #[test]
    fn test_round_trip_with_header() {
        let content = "```toml\nfoo = \"bar\"\n```\n\n---\n\nActual content";

        // 1. Load
        let (e, mut fs) = setup_and_load(content);

        // 2. Write to a new location
        fs.create_dir_all(&PathBuf::from("bar")).unwrap();
        let writer = EntityWriter {};
        writer
            .write_entity(&mut fs, &PathBuf::from("bar"), &e)
            .unwrap();

        // 3. Load from new location and verify
        let e2 = load_entity(&fs, "bar", "entity1");
        assert_eq!(e, e2);

        // Verify file content exactly
        let file_content = fs.get_str(&PathBuf::from("bar/entity1.md")).unwrap();
        assert_eq!(file_content, content);
    }


    #[test]
    fn test_round_trip_with_yaml_header() {
        let content = "---\nfoo: bar\n---\n\nActual content";

        // 1. Load
        let (e, mut fs) = setup_and_load(content);

        // 2. Write to a new location
        fs.create_dir_all(&PathBuf::from("bar")).unwrap();
        let writer = EntityWriter {};
        writer
            .write_entity(&mut fs, &PathBuf::from("bar"), &e)
            .unwrap();

        // 3. Load from new location and verify
        let e2 = load_entity(&fs, "bar", "entity1");
        assert_eq!(e, e2);
    }

    #[test]
    fn test_load_entity_with_various_thematic_breaks() {
        let mut fs = mockfs::MockFS::new();
        let breaks = vec!["---", "***", "___", " - - -", "  ***  ", "   ___ ___ ___"];
        for (i, b) in breaks.iter().enumerate() {
            let name = format!("entity{}", i);
            let content = format!("```toml\nkey = \"{i}\"\n```\n{b}\nContent");
            create_file_with_content(&mut fs, "foo", format!("{}.md", name), &content);
        }

        for (i, _) in breaks.iter().enumerate() {
            let e = load_entity(&fs, "foo", &format!("entity{}", i));
            check_header_meta(&e.metadata, "key", &i.to_string(), Some(&format!("{}\n", breaks[i])));
        }
    }

    #[test]
    fn test_load_entity_auto_type_success() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "type=\"TestType\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "Auto")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_auto_type_no_metadata_errors() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Hello");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let result = loader.try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "Auto");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("has Auto type but no metadata"));
    }

    #[test]
    fn test_load_entity_auto_type_missing_type_key_errors() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "bar=\"baz\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let result = loader.try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "Auto");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("missing 'type' key"));
    }

    #[test]
    fn test_load_entity_auto_type_resolves_to_auto_errors() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "type=\"Auto\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let result = loader.try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "Auto");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("set to 'Auto', which is not allowed"));
    }

    #[test]
    fn test_load_entity_concrete_type_matches_metadata_success() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "type=\"TestType\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.node_type, "TestType".to_string());
    }

    #[test]
    fn test_load_entity_concrete_type_mismatch_metadata_errors() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "type=\"OtherType\"\n");
        let fs = fs;

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");

        let result = loader.try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("has type 'OtherType' in metadata, but was expected to be 'TestType'"));
    }

    #[test]
    fn test_load_child_auto_type_success() {
        let mut fs = mockfs::MockFS::new();
        // parent (TestType) -> child (Auto -> ChildTestType)
        create_file_with_content(
            &mut fs,
            "foo/parent",
            "child1.meta.toml",
            "type=\"ChildTestType\"\n",
            );
            let fs = fs;

        let mut loader = EntityLoader::new();
        loader.schema.entity_types.insert(
            "TestType".to_string(),
            EntityTypeDescription {
                name: "TestType".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^child.*$".to_string(),
                    node_type: "Auto".to_string(),
                    required: false,
                    edge: Edge::Slash,
                    multiple: true,
                }],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        );
        loader.schema.entity_types.insert(
            "ChildTestType".to_string(),
            EntityTypeDescription {
                name: "ChildTestType".to_string(),
                children: vec![],
                allow_additional: false,
                layout: None,
                ignore: vec![],
            },
        );

        let entity_path = EntityPath::empty().extend_slash("parent");
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.children.len(), 1);
        assert_eq!(e.children[0].node_type, "ChildTestType");
    }

    #[test]
    fn test_load_root_auto_type_success() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type=\"Project\"\n");
        let fs = fs;

        let mut loader = EntityLoader::new();
        loader.schema.entity_types.insert(
            "Project".to_string(),
            EntityTypeDescription {
                name: "Project".to_string(),
                children: vec![],
                allow_additional: true,
                layout: None,
                ignore: vec![],
            },
        );

        let entity_path = EntityPath::empty();
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &entity_path, "Auto")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.node_type, "Project".to_string());
    }

    #[test]
    fn test_ignore_slash_child() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/010_chapter", "content.md", "chapter");
        create_file_with_content(&mut fs, "project/booker-data", "state.json", "{}");

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^[0-9]+_".to_string(),
                node_type: "Chapter".to_string(),
                required: false,
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec!["booker-data".to_string()],
        }).unwrap();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &EntityPath::empty(), "Project")
            .unwrap()
            .unwrap();
        assert_eq!(entity.children.len(), 1);
        assert_eq!(entity.children[0].node_type, "Chapter");
    }

    #[test]
    fn test_ignore_dot_child() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project/parent", "content.md", "parent");
        create_file_with_content(&mut fs, "project", "parent.child.md", "child content");
        create_file_with_content(&mut fs, "project", "parent.booker-data.md", "tool data");

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^child$".to_string(),
                node_type: "Child".to_string(),
                required: false,
                edge: Edge::Slash,
                multiple: false,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec!["booker-data".to_string()],
        }).unwrap();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();

        let entity_path = EntityPath::empty().extend_slash("parent");
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &entity_path, "Parent")
            .unwrap()
            .unwrap();
        assert_eq!(entity.children.len(), 1);
        assert_eq!(entity.children[0].node_type, "Child");
    }

    #[test]
    fn test_ignore_multiple_entries() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/010_chapter", "content.md", "chapter");
        create_file_with_content(&mut fs, "project/booker-data", "state.json", "{}");
        create_file_with_content(&mut fs, "project/cache", "data.bin", "");

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^[0-9]+_".to_string(),
                node_type: "Chapter".to_string(),
                required: false,
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec!["booker-data".to_string(), "cache".to_string()],
        }).unwrap();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();

        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &EntityPath::empty(), "Project")
            .unwrap()
            .unwrap();
        assert_eq!(entity.children.len(), 1);
        assert_eq!(entity.children[0].node_type, "Chapter");
    }
}

// #[test]
// pub fn test_write_entity_with_content() {
//     let content = "Hello, world!";
//     let data = EntityData {
//         content: Some(content.to_string()),
//         children: vec![],
//         filename: PathBuf::from("an_entity"),
//         metadata: None,
//         node_type: String::from("TestType"),
//     };
//     let entity = Entity::File(FileEntity { data });
//     let writer = EntityWriter {};
//     let mut fs = mockfs::MockFS::new();
//     fs.create_dir(&PathBuf::from("foo")).unwrap();
//     writer
//         .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
//         .unwrap();
//     // Now check that the file exists with the content file.
//     let file_entity_path = fs.resolve_path(&PathBuf::from("foo/an_entity.md")).unwrap();
//     let file_entity = file_entity_path.as_file().unwrap();
//     let file_content = std::str::from_utf8(file_entity.contents.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(file_content, content);
// }

// #[test]
// pub fn test_write_entity_with_metadata() {
//     let metadata = Metadata {
//         value: toml::from_str("bar = \"foo\"\n").unwrap(),
//     };
//     let data = EntityData {
//         content: None,
//         children: vec![],
//         filename: PathBuf::from("an_entity"),
//         metadata: Some(metadata),
//         node_type: String::from("TestType"),
//     };
//     let entity = Entity::File(FileEntity { data });
//     let writer = EntityWriter {};
//     let mut fs = mockfs::MockFS::new();
//     fs.create_dir(&PathBuf::from("foo")).unwrap();
//     writer
//         .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
//         .unwrap();
//     // Now check that the file exists with the metadata file.
//     let file_metadata_path = fs
//         .resolve_path(&PathBuf::from("foo/an_entity.meta.toml"))
//         .unwrap();
//     let file_metadata = file_metadata_path.as_file().unwrap();
//     let file_metadata_content = std::str::from_utf8(file_metadata.contents.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(file_metadata_content, "bar = \"foo\"\n");
// }

// #[test]
// pub fn test_write_entity_with_file_children() {
//     let child1_data = EntityData {
//         content: Some("Child 1 content".to_string()),
//         children: vec![],
//         filename: PathBuf::from("an_entity.child1"),
//         metadata: None,
//         node_type: String::from("ChildTestType"),
//     };
//     let child2_data = EntityData {
//         content: Some("Child 2 content".to_string()),
//         children: vec![],
//         filename: PathBuf::from("an_entity.child2"),
//         metadata: None,
//         node_type: String::from("ChildTestType"),
//     };
//     let data = EntityData {
//         content: None,
//         children: vec![
//             Entity::File(FileEntity { data: child1_data }),
//             Entity::File(FileEntity { data: child2_data }),
//         ],
//         filename: PathBuf::from("an_entity"),
//         metadata: None,
//         node_type: String::from("TestType"),
//     };
//     let entity = Entity::File(FileEntity { data });
//     let writer = EntityWriter {};
//     let mut fs = mockfs::MockFS::new();
//     fs.create_dir(&PathBuf::from("foo")).unwrap();
//     writer
//         .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
//         .unwrap();
//     // Now check that the files exist with the child files.
//     let child1_file = fs
//         .resolve_path(&PathBuf::from("foo/an_entity.child1.md"))
//         .unwrap();
//     let child1_file = child1_file.as_file().unwrap();
//     let child1_content = std::str::from_utf8(child1_file.contents.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(child1_content, "Child 1 content");
//     let child2_file = fs
//         .resolve_path(&PathBuf::from("foo/an_entity.child2.md"))
//         .unwrap();
//     let child2_file = child2_file.as_file().unwrap();
//     let child2_content = std::str::from_utf8(child2_file.contents.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(child2_content, "Child 2 content");
// }

// pub fn test_write_entity_with_directory_children() {
//     let child1_data = EntityData {
//         content: Some("Child 1 content".to_string()),
//         children: vec![],
//         filename: PathBuf::from("child1"),
//         metadata: None,
//         node_type: String::from("ChildTestType"),
//     };
//     let child2_data = EntityData {
//         content: Some("Child 2 content".to_string()),
//         children: vec![],
//         filename: PathBuf::from("child2"),
//         metadata: None,
//         node_type: String::from("ChildTestType"),
//     };
//     let data = EntityData {
//         content: None,
//         children: vec![
//             Entity::Directory(DirectoryEntity { data: child1_data }),
//             Entity::Directory(DirectoryEntity { data: child2_data }),
//         ],
//         filename: PathBuf::from("an_entity"),
//         metadata: None,
//         node_type: String::from("TestType"),
//     };
//     let entity = Entity::File(FileEntity { data });
//     let writer = EntityWriter {};
//     let mut fs = mockfs::MockFS::new();
//     fs.create_dir(&PathBuf::from("foo")).unwrap();
//     writer
//         .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
//         .unwrap();
//     // Now check that the files exist with the child directories.
//     let child1_de = fs
//         .resolve_path(&PathBuf::from("foo/an_entity/child1"))
//         .unwrap();
//     let child1_de = child1_de.as_dir().unwrap();
//     assert_eq!(child1_de.entries.len(), 1);
//     let child1_content = &fs
//         .resolve_path(&PathBuf::from("foo/an_entity/child1/content.md"))
//         .unwrap()
//         .as_file()
//         .unwrap()
//         .contents;
//     let child1_content = std::str::from_utf8(child1_content.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(child1_content, "Child 1 content");
//     let child2_de = fs
//         .resolve_path(&PathBuf::from("foo/an_entity/child2"))
//         .unwrap();
//     let child2_de = child2_de.as_dir().unwrap();
//     assert_eq!(child2_de.entries.len(), 1);
//     let child2_content = &fs
//         .resolve_path(&PathBuf::from("foo/an_entity/child2/content.md"))
//         .unwrap()
//         .as_file()
//         .unwrap()
//         .contents;
//     let child2_content = std::str::from_utf8(child2_content.borrow().as_slice())
//         .unwrap()
//         .to_string();
//     assert_eq!(child2_content, "Child 2 content");
// }
