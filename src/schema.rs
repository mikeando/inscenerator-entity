use std::collections::HashMap;
use std::path::Path;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use crate::entity::{Entity, EntityLoader, EntityPath};
use inscenerator_xfs::Xfs;

/// Rules for discovering and validating child entities.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    /// Regex pattern to match the child entity name.
    pub name_regex: String,
    /// The expected entity type for matching children.
    pub node_type: String,
    /// Whether this child is required.
    pub required: bool,
    /// Whether multiple children of this type are allowed.
    pub multiple: bool,
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
}

/// A collection of entity type descriptions.
#[derive(Default, Clone, Debug)]
pub struct Schema {
    /// Map of entity type names to their descriptions.
    pub entity_types: HashMap<String, EntityTypeDescription>,
}

#[derive(Deserialize)]
struct RawEntityTypeDescription {
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
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
    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// Adds a new entity type description to the schema.
    pub fn add_entity_type(&mut self, description: EntityTypeDescription) {
        self.entity_types.insert(description.name.clone(), description);
    }

    /// Loads a schema from a TOML file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    pub fn load_from_file(fs: &dyn Xfs, path: &Path) -> anyhow::Result<Self> {
        let content = crate::entity::utils::try_load_file_as_string(fs, path)?
            .ok_or_else(|| anyhow!("Schema file not found at {:?}", path))?;
        let raw_entities: HashMap<String, RawEntityTypeDescription> = toml::from_str(&content)?;
        let entity_types = raw_entities
            .into_iter()
            .map(|(name, raw)| {
                (
                    name.clone(),
                    EntityTypeDescription {
                        name,
                        children: raw.children,
                        allow_additional: raw.allow_additional,
                    },
                )
            })
            .collect();
        Ok(Schema { entity_types })
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

    let root = loader.try_load_entity(fs, root_path, &EntityPath::empty(), "Auto")?
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

    #[test]
    fn test_load_schema_from_toml() {
        let mut fs = mockfs::MockFS::new();
        let toml_content = r#"
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
        create_file_with_content(&mut fs, "project", "schema.toml", toml_content);

        let schema = Schema::load_from_file(&fs, &Path::new("project/schema.toml")).unwrap();
        assert_eq!(schema.entity_types.len(), 2);
        assert_eq!(schema.entity_types["Project"].name, "Project");
        assert_eq!(schema.entity_types["Project"].children.len(), 1);
        assert_eq!(schema.entity_types["Project"].children[0].node_type, "Chapter");
        assert_eq!(schema.entity_types["Chapter"].name, "Chapter");
        assert!(schema.entity_types["Chapter"].allow_additional);
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
        assert_eq!(schema.entity_types.len(), 2);
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
}
