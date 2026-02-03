use std::collections::HashMap;
use std::path::Path;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use crate::entity::{Entity, EntityLoader, EntityPath};
use inscenerator_xfs::Xfs;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    pub name_regex: String,
    pub node_type: String,
    pub required: bool,
    pub multiple: bool,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EntityTypeDescription {
    pub name: String,
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
}

#[derive(Debug, Clone)]
pub struct Schema {
    pub entities: HashMap<String, EntityTypeDescription>,
}

#[derive(Deserialize)]
struct RawEntityTypeDescription {
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
}

impl Schema {
    pub fn load_from_file(fs: &dyn Xfs, path: &Path) -> anyhow::Result<Self> {
        let content = crate::entity::utils::try_load_file_as_string(fs, path)?
            .ok_or_else(|| anyhow!("Schema file not found at {:?}", path))?;
        let raw_entities: HashMap<String, RawEntityTypeDescription> = toml::from_str(&content)?;
        let entities = raw_entities
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
        Ok(Schema { entities })
    }
}

pub fn load_schema_and_root(fs: &dyn Xfs, root_path: &Path) -> anyhow::Result<(Schema, Entity)> {
    let schema_path = root_path.join("schema.toml");
    let schema = Schema::load_from_file(fs, &schema_path)?;

    let mut loader = EntityLoader::new();
    loader.entity_types = schema.entities.clone();

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
        assert_eq!(schema.entities.len(), 2);
        assert_eq!(schema.entities["Project"].name, "Project");
        assert_eq!(schema.entities["Project"].children.len(), 1);
        assert_eq!(schema.entities["Project"].children[0].node_type, "Chapter");
        assert_eq!(schema.entities["Chapter"].name, "Chapter");
        assert!(schema.entities["Chapter"].allow_additional);
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
        assert_eq!(schema.entities.len(), 2);
        assert_eq!(root.node_type, "Project");
        assert_eq!(root.children.len(), 1);
        assert_eq!(root.children[0].node_type, "Chapter");
    }
}
