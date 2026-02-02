use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, Context};
use serde::{Deserialize, Serialize};
use inscenerator_xfs::Xfs;

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

mod utils {
    use std::collections::BTreeSet;

    use super::*;
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
            if suffix == "md" {
                continue;
            }
            if suffix == "meta.toml" {
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
                if full_filename == "content.md" || full_filename == "meta.toml" {
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ChildEntityRules {
    pub name_regex: String,
    pub node_type: String,
    pub required: bool,
    pub multiple: bool,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EntityTypeDescription {
    pub name: String,
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
}

pub struct EntityLoader {
    pub entity_types: HashMap<String, EntityTypeDescription>,
}

impl EntityLoader {
    pub fn new() -> EntityLoader {
        EntityLoader {
            entity_types: HashMap::new(),
        }
    }

    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow::anyhow!("Invalid entity type {}", entity_type))
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
        let dot_content = if !is_root {
            utils::try_load_file_as_string(fs, &dot_content_file)
        } else {
            Ok(None)
        };
        let slash_content = utils::try_load_file_as_string(fs, &slash_content_file);
        let content: EntityContent = match (dot_content?, slash_content?) {
            (Some(_), Some(_)) => bail!(
                "Both {} and {} exist.",
                dot_content_file.display(),
                slash_content_file.display()
            ),
            (Some(c), None) => EntityContent::Parallel(c),
            (None, Some(c)) => EntityContent::Inside(c),
            (None, None) => EntityContent::None,
        };

        // Try loading the metadata
        let dot_metadata_file = entity_path
            .to_pathbuf(base_path)
            .with_extension("meta.toml");
        let slash_metadata_file = entity_path.to_pathbuf(base_path).join("meta.toml");
        let dot_metadata = if !is_root {
            utils::try_load_file_as_metadata(fs, &dot_metadata_file)
        } else {
            Ok(None)
        };
        let slash_metadata = utils::try_load_file_as_metadata(fs, &slash_metadata_file);
        let metadata = match (dot_metadata?, slash_metadata?) {
            (Some(_), Some(_)) => bail!(
                "Both {} and {} exist.",
                dot_metadata_file.display(),
                slash_metadata_file.display()
            ),
            (Some(m), None) => EntityMeta::Parallel(m),
            (None, Some(m)) => EntityMeta::Inside(m),
            (None, None) => EntityMeta::None,
        };

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
            let m = metadata.metadata().ok_or_else(|| {
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
            if let Some(m) = metadata.metadata() {
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
            || entity.metadata.is_inside()
            || entity
                .children
                .iter()
                .any(|c: &Entity| c.path.entries.last().unwrap().is_slash());
        if needs_directory {
            fs.create_dir_all(&entity_path)?;
        }

        match &entity.content {
            EntityContent::Parallel(content) => {
                let dot_content_file = entity_path.with_added_extension("md");
                fs.writer(&dot_content_file)?
                    .write_all(content.as_bytes())?;
            }
            EntityContent::Inside(content) => {
                fs.writer(&entity_path.join("content.md"))?
                    .write_all(content.as_bytes())?;
            }
            EntityContent::None => {}
        }

        match &entity.metadata {
            EntityMeta::Parallel(metadata) => {
                let toml_str = toml::to_string(&metadata.value)?;
                let dot_metadata_file = entity_path.with_extension("meta.toml");
                fs.writer(&dot_metadata_file)?
                    .write_all(toml_str.as_bytes())?;
            }
            EntityMeta::Inside(metadata) => {
                let toml_str = toml::to_string(&metadata.value)?;
                fs.writer(&entity_path.join("meta.toml"))?
                    .write_all(toml_str.as_bytes())?;
            }
            EntityMeta::None => {}
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

#[derive(Debug, PartialEq, Clone)]
pub enum EntityMeta {
    None,
    /// metadata is found at entity.meta.toml
    Parallel(Metadata),
    /// metadata is found at entity/meta.toml
    Inside(Metadata),
}

impl EntityMeta {
    pub fn is_none(&self) -> bool {
        matches!(self, EntityMeta::None)
    }

    pub fn metadata(&self) -> Option<&Metadata> {
        match self {
            EntityMeta::None => None,
            EntityMeta::Parallel(m) => Some(m),
            EntityMeta::Inside(m) => Some(m),
        }
    }

    pub fn parallel(m: Metadata) -> EntityMeta {
        EntityMeta::Parallel(m)
    }

    pub fn inside(m: Metadata) -> EntityMeta {
        EntityMeta::Inside(m)
    }

    fn is_inside(&self) -> bool {
        matches!(self, EntityMeta::Inside(_))
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

#[cfg(test)]
mod common {
    use super::*;

    pub fn dummy_loader() -> EntityLoader {
        let mut loader = EntityLoader::new();
        loader.entity_types.insert(
            "TestType".to_string(),
            EntityTypeDescription {
                name: "TestType".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^child[0-9]*$".to_string(),
                    node_type: "ChildTestType".to_string(),
                    required: false,
                    multiple: true,
                }],
                allow_additional: false,
            },
        );
        loader.entity_types.insert(
            "ChildTestType".to_string(),
            EntityTypeDescription {
                name: "ChildTestType".to_string(),
                children: vec![],
                allow_additional: false,
            },
        );
        loader
    }
}

#[cfg(test)]
mod entity_tests {

    use inscenerator_xfs::mockfs;

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
            EntityMeta::Inside(Metadata {
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
            EntityMeta::Parallel(Metadata {
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

        loader.entity_types.insert(
            "Project".to_string(),
            EntityTypeDescription {
                name: "Project".to_string(),
                children: vec![
                    ChildEntityRules {
                        name_regex: "^[0-9]+_".to_string(),
                        node_type: "Chapter".to_string(),
                        required: false,
                        multiple: true,
                    },
                    ChildEntityRules {
                        name_regex: "^notes$".to_string(),
                        node_type: "Notes".to_string(),
                        required: false,
                        multiple: true,
                    },
                ],
                allow_additional: false,
            },
        );
        loader.entity_types.insert(
            "Chapter".to_string(),
            EntityTypeDescription {
                name: "Chapter".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^[0-9]+_".to_string(),
                    node_type: "Scene".to_string(),
                    required: false,
                    multiple: true,
                }],
                allow_additional: false,
            },
        );
        loader.entity_types.insert(
            "Scene".to_string(),
            EntityTypeDescription {
                name: "Scene".to_string(),
                children: vec![],
                allow_additional: true,
            },
        );
        loader.entity_types.insert(
            "Notes".to_string(),
            EntityTypeDescription {
                name: "Notes".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: ".*".to_string(),
                    node_type: "Notes".to_string(),
                    required: false,
                    multiple: true,
                }],
                allow_additional: true,
            },
        );

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
            metadata: EntityMeta::None,
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
        assert_eq!(de.entries.len(), 1);
        let file_content = &fs
            .resolve_path(&PathBuf::from("foo/an_entity/content.md"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let file_content = std::str::from_utf8(file_content.borrow().as_slice())
            .unwrap()
            .to_string();
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
            metadata: EntityMeta::None,
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        // Now check that the content file.
        let file_content = &fs
            .resolve_path(&PathBuf::from("foo/an_entity.md"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let file_content = std::str::from_utf8(file_content.borrow().as_slice())
            .unwrap()
            .to_string();
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
            metadata: EntityMeta::Inside(metadata),
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
        assert_eq!(de.entries.len(), 1);
        let file_metadata = &fs
            .resolve_path(&PathBuf::from("foo/an_entity/meta.toml"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let file_metadata = std::str::from_utf8(file_metadata.borrow().as_slice())
            .unwrap()
            .to_string();
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
            metadata: EntityMeta::None,
            node_type: String::from("ChildTestType"),
        };
        let child2 = Entity {
            content: EntityContent::inside("Child 2 content".to_string()),
            children: vec![],
            path: entity_path.extend_slash("child2"),
            metadata: EntityMeta::None,
            node_type: String::from("ChildTestType"),
        };
        let entity = Entity {
            content: EntityContent::None,
            children: vec![child1, child2],
            path: entity_path,
            metadata: EntityMeta::None,
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
        assert_eq!(de.entries.len(), 2);
        let child1_de = fs
            .resolve_path(&PathBuf::from("foo/an_entity/child1"))
            .unwrap();
        let child1_de = child1_de.as_dir().unwrap();
        assert_eq!(child1_de.entries.len(), 1);
        let child1_content = &fs
            .resolve_path(&PathBuf::from("foo/an_entity/child1/content.md"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let child1_content = std::str::from_utf8(child1_content.borrow().as_slice())
            .unwrap()
            .to_string();
        assert_eq!(child1_content, "Child 1 content");
        let child2_de = fs
            .resolve_path(&PathBuf::from("foo/an_entity/child2"))
            .unwrap();
        let child2_de = child2_de.as_dir().unwrap();
        assert_eq!(child2_de.entries.len(), 1);
        let child2_content = &fs
            .resolve_path(&PathBuf::from("foo/an_entity/child2/content.md"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let child2_content = std::str::from_utf8(child2_content.borrow().as_slice())
            .unwrap()
            .to_string();
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
            metadata: EntityMeta::None,
            node_type: String::from("ChildTestType"),
        };
        let child2 = Entity {
            content: EntityContent::parallel("Child 2 content".to_string()),
            children: vec![],
            path: entity_path.extend_dot("child2"),
            metadata: EntityMeta::None,
            node_type: String::from("ChildTestType"),
        };
        let entity = Entity {
            content: EntityContent::None,
            children: vec![child1, child2],
            path: entity_path,
            metadata: EntityMeta::None,
            node_type: String::from("TestType"),
        };
        let writer = EntityWriter {};
        let mut fs = mockfs::MockFS::new();
        fs.create_dir(&PathBuf::from("foo")).unwrap();
        writer
            .write_entity(&mut fs, &PathBuf::from("foo"), &entity)
            .unwrap();
        let child1_content_file = fs
            .resolve_path(&PathBuf::from("foo/an_entity.child1.md"))
            .unwrap();
        let child1_file = child1_content_file.as_file().unwrap();
        let child1_content = std::str::from_utf8(child1_file.contents.borrow().as_slice())
            .unwrap()
            .to_string();
        assert_eq!(child1_content, "Child 1 content");
        let child2_content_file = fs
            .resolve_path(&PathBuf::from("foo/an_entity.child2.md"))
            .unwrap();
        let child2_file = child2_content_file.as_file().unwrap();
        let child2_content = std::str::from_utf8(child2_file.contents.borrow().as_slice())
            .unwrap()
            .to_string();
        assert_eq!(child2_content, "Child 2 content");
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
        loader.entity_types.insert(
            "TestType".to_string(),
            EntityTypeDescription {
                name: "TestType".to_string(),
                children: vec![ChildEntityRules {
                    name_regex: "^child.*$".to_string(),
                    node_type: "Auto".to_string(),
                    required: false,
                    multiple: true,
                }],
                allow_additional: false,
            },
        );
        loader.entity_types.insert(
            "ChildTestType".to_string(),
            EntityTypeDescription {
                name: "ChildTestType".to_string(),
                children: vec![],
                allow_additional: false,
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
        loader.entity_types.insert(
            "Project".to_string(),
            EntityTypeDescription {
                name: "Project".to_string(),
                children: vec![],
                allow_additional: true,
            },
        );

        let entity_path = EntityPath::empty();
        let entity = loader
            .try_load_entity(&fs, &PathBuf::from("project"), &entity_path, "Auto")
            .unwrap();
        let e = entity.unwrap();
        assert_eq!(e.node_type, "Project".to_string());
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
