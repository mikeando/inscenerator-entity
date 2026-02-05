use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::cell::RefCell;

use anyhow::{anyhow, bail, Context};
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

    pub fn parse_header(content: &str) -> Option<(Metadata, Option<String>, String)> {
        let thematic_break_re =
            regex::Regex::new(r"^[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:_[ \t]*){3,}|(?:\*[ \t]*){3,})[ \t]*$")
                .unwrap();

        // 1. Find the opening ```
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

        // 2. Find the closing ```
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
        let inner_toml_trimmed = inner_toml.trim_end_matches(['\n', '\r']);

        let value: toml::Value = toml::from_str(inner_toml_trimmed).ok()?;
        let metadata = Metadata { value };

        // 3. Find optional separator
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
                // Not a thematic break and not empty -> end of gap, no separator found
                break;
            }
            current_gap.push_str(line);
        }

        let actual_content = content[content_start_pos..].to_string();

        Some((metadata, separator, actual_content))
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

mod xfs_ext {
    use super::*;
    pub fn remove_file(_fs: &mut dyn Xfs, _path: &Path) -> anyhow::Result<()> {
        todo!("remove_file not yet in Xfs trait")
    }
    pub fn remove_dir_all(_fs: &mut dyn Xfs, _path: &Path) -> anyhow::Result<()> {
        todo!("remove_dir_all not yet in Xfs trait")
    }
    pub fn rename(_fs: &mut dyn Xfs, _from: &Path, _to: &Path) -> anyhow::Result<()> {
        todo!("rename not yet in Xfs trait")
    }
}

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
        let entity_type_descriptor = self.get_entity_type(entity_type)?;

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
                .map(|(m, s, a)| (Some(EntityMeta::InHeader(m, s)), a))
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
            meta_sources.push(EntityMeta::Parallel(m));
        }
        if let Some(m) = slash_metadata {
            meta_sources.push(EntityMeta::Inside(m));
        }
        if let Some(m) = metadata_from_content {
            meta_sources.push(m);
        }

        if meta_sources.len() > 1 {
            bail!(
                "Multiple metadata sources found for entity at {:?}.",
                entity_path.to_pathbuf(base_path)
            );
        }
        let metadata = meta_sources.into_iter().next().unwrap_or(EntityMeta::None);

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
            if !found_match && !self.get_entity_type(entity_type)?.allow_additional {
                bail!(
                    "Unexpected child entity '{}' in entity '{:?}'",
                    child_name,
                    entity_path.to_pathbuf(base_path)
                );
            }
        }

        if content.is_none() && metadata.is_none() && !has_children && !directory_exists {
            return Ok(None);
        }

        let entity = Entity {
            path: entity_path.clone(),
            node_type: entity_type.to_string(),
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

        if let Some(content) = entity.content.content() {
            let mut to_write = String::new();
            if let EntityMeta::InHeader(m, sep) = &entity.metadata {
                to_write.push_str("```toml\n");
                to_write.push_str(&toml::to_string(&m.value)?);
                to_write.push_str("```\n");
                if let Some(s) = sep {
                    to_write.push_str(s);
                } else if !content.starts_with('\n') {
                    to_write.push_str("\n");
                }
            }
            to_write.push_str(content);

            let path = match &entity.content {
                EntityContent::Parallel(_) => entity_path.with_added_extension("md"),
                EntityContent::Inside(_) => entity_path.join("content.md"),
                _ => unreachable!(),
            };
            fs.writer(&path)?.write_all(to_write.as_bytes())?;
        } else if let EntityMeta::InHeader(_, _) = &entity.metadata {
            bail!(
                "Metadata from header requires content for entity at {:?}",
                entity.path.to_pathbuf(base_path)
            );
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
            EntityMeta::None | EntityMeta::InHeader(_, _) => {}
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
    /// metadata is found in the content file
    InHeader(Metadata, Option<String>),
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
            EntityMeta::InHeader(m, _) => Some(m),
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

pub struct LiveEntity {
    pub fs: Rc<RefCell<dyn Xfs>>,
    pub base_path: PathBuf,
    pub path: EntityPath,
    pub node_type: String,
    pub schema: Rc<Schema>,
}

impl LiveEntity {
    pub fn new(
        fs: Rc<RefCell<dyn Xfs>>,
        base_path: PathBuf,
        path: EntityPath,
        node_type: String,
        schema: Rc<Schema>,
    ) -> Self {
        Self {
            fs,
            base_path,
            path,
            node_type,
            schema,
        }
    }

    pub fn path(&self) -> &EntityPath {
        &self.path
    }

    pub fn node_type(&self) -> &str {
        &self.node_type
    }

    fn on_disk_path(&self) -> PathBuf {
        self.path.to_pathbuf(&self.base_path)
    }

    fn dot_content_path(&self) -> PathBuf {
        self.on_disk_path().with_added_extension("md")
    }

    fn slash_content_path(&self) -> PathBuf {
        self.on_disk_path().join("content.md")
    }

    fn dot_metadata_path(&self) -> PathBuf {
        self.on_disk_path().with_extension("meta.toml")
    }

    fn slash_metadata_path(&self) -> PathBuf {
        self.on_disk_path().join("meta.toml")
    }

    pub fn content(&self) -> anyhow::Result<EntityContent> {
        let fs = self.fs.borrow();
        let is_root = self.path.entries.is_empty();

        let dot_content_file = self.dot_content_path();
        let slash_content_file = self.slash_content_path();

        let (content_str, is_parallel) = if !is_root {
            if let Some(c) = utils::try_load_file_as_string(&*fs, &dot_content_file)? {
                if fs.is_file(&slash_content_file) {
                    bail!(
                        "Both {} and {} exist.",
                        dot_content_file.display(),
                        slash_content_file.display()
                    );
                }
                (Some(c), true)
            } else {
                (
                    utils::try_load_file_as_string(&*fs, &slash_content_file)?,
                    false,
                )
            }
        } else {
            (
                utils::try_load_file_as_string(&*fs, &slash_content_file)?,
                false,
            )
        };

        let content = if let Some(c) = content_str {
            let (_, a) = utils::parse_header(&c)
                .map(|(m, s, a)| (Some(EntityMeta::InHeader(m, s)), a))
                .unwrap_or((None, c));
            if is_parallel {
                EntityContent::Parallel(a)
            } else {
                EntityContent::Inside(a)
            }
        } else {
            EntityContent::None
        };

        Ok(content)
    }

    pub fn metadata(&self) -> anyhow::Result<EntityMeta> {
        let fs = self.fs.borrow();
        let is_root = self.path.entries.is_empty();

        // 1. Try loading from content header
        let dot_content_file = self.dot_content_path();
        let slash_content_file = self.slash_content_path();

        let content_str = if !is_root {
            if let Some(c) = utils::try_load_file_as_string(&*fs, &dot_content_file)? {
                Some(c)
            } else {
                utils::try_load_file_as_string(&*fs, &slash_content_file)?
            }
        } else {
            utils::try_load_file_as_string(&*fs, &slash_content_file)?
        };

        let metadata_from_content = content_str.and_then(|c| {
            utils::parse_header(&c).map(|(m, s, _)| EntityMeta::InHeader(m, s))
        });

        // 2. Try loading from meta.toml files
        let dot_metadata_file = self.dot_metadata_path();
        let slash_metadata_file = self.slash_metadata_path();

        let dot_metadata = if !is_root {
            utils::try_load_file_as_metadata(&*fs, &dot_metadata_file)?
        } else {
            None
        };
        let slash_metadata = utils::try_load_file_as_metadata(&*fs, &slash_metadata_file)?;

        let mut meta_sources = Vec::new();
        if let Some(m) = dot_metadata {
            meta_sources.push(EntityMeta::Parallel(m));
        }
        if let Some(m) = slash_metadata {
            meta_sources.push(EntityMeta::Inside(m));
        }
        if let Some(m) = metadata_from_content {
            meta_sources.push(m);
        }

        if meta_sources.len() > 1 {
            bail!(
                "Multiple metadata sources found for entity at {:?}.",
                self.on_disk_path()
            );
        }

        Ok(meta_sources.into_iter().next().unwrap_or(EntityMeta::None))
    }

    pub fn children(&self) -> anyhow::Result<Vec<LiveEntity>> {
        let fs = self.fs.borrow();
        let is_root = self.path.entries.is_empty();

        let dot_children = if !is_root {
            utils::find_dot_children(&*fs, &self.base_path, &self.path)?
        } else {
            vec![]
        };
        let slash_children = utils::find_slash_children(&*fs, &self.base_path, &self.path)?;
        let children_paths = dot_children
            .into_iter()
            .chain(slash_children)
            .collect::<Vec<EntityPath>>();

        let entity_type_descriptor = self.schema.get_entity_type(&self.node_type)?;

        let mut loaded_children = vec![];
        for child_path in children_paths {
            let child_name = child_path.last_name().unwrap();
            let mut found_match = false;
            for child_rule in &entity_type_descriptor.children {
                let re = regex::Regex::new(&child_rule.name_regex).unwrap();
                if re.is_match(child_name) {
                    loaded_children.push(LiveEntity::new(
                        self.fs.clone(),
                        self.base_path.clone(),
                        child_path.clone(),
                        child_rule.node_type.clone(),
                        self.schema.clone(),
                    ));
                    found_match = true;
                    break;
                }
            }
            if !found_match && !entity_type_descriptor.allow_additional {
                // In LiveEntity, maybe we should just ignore or return as "Unknown" type?
                // The current EntityLoader bails.
                bail!(
                    "Unexpected child entity '{}' in entity '{:?}'",
                    child_name,
                    self.on_disk_path()
                );
            }
        }

        Ok(loaded_children)
    }

    pub fn set_content(&self, new_content: &str) -> anyhow::Result<()> {
        let current_meta = self.metadata()?;
        let current_content_type = self.content()?;

        let mut to_write = String::new();
        if let EntityMeta::InHeader(m, sep) = current_meta {
            to_write.push_str("```toml\n");
            to_write.push_str(&toml::to_string(&m.value)?);
            to_write.push_str("```\n");
            if let Some(s) = sep {
                to_write.push_str(&s);
            } else if !new_content.starts_with('\n') {
                to_write.push_str("\n");
            }
        }
        to_write.push_str(new_content);

        let path = match current_content_type {
            EntityContent::Parallel(_) => self.dot_content_path(),
            EntityContent::Inside(_) => self.slash_content_path(),
            EntityContent::None => {
                let fs = self.fs.borrow();
                if self.path.entries.is_empty() || fs.is_dir(&self.on_disk_path()) {
                    self.slash_content_path()
                } else {
                    self.dot_content_path()
                }
            }
        };

        let mut fs = self.fs.borrow_mut();
        if let Some(parent) = path.parent() {
            fs.create_dir_all(parent)?;
        }
        let mut writer = fs.writer(&path)?;
        writer.write_all(to_write.as_bytes())?;
        Ok(())
    }

    pub fn set_metadata(&self, meta: EntityMeta) -> anyhow::Result<()> {
        // If we are changing TO or FROM InHeader, we might need to modify the content file.
        let current_meta = self.metadata()?;
        let current_content = self.content()?;

        let mut fs = self.fs.borrow_mut();

        // If current was InHeader, and new is not, we need to strip it from the file.
        if let EntityMeta::InHeader(_, _) = current_meta {
            if !matches!(meta, EntityMeta::InHeader(_, _)) {
                // Strip header from content file
                let path = match current_content {
                    EntityContent::Parallel(_) => self.dot_content_path(),
                    EntityContent::Inside(_) => self.slash_content_path(),
                    _ => unreachable!("InHeader implies content exists"),
                };
                let content_str = self.content()?.content().unwrap().to_string();
                let mut writer = fs.writer(&path)?;
                writer.write_all(content_str.as_bytes())?;
            }
        }

        match meta {
            EntityMeta::None => {
                // Remove existing metadata files if they exist?
                // For now just don't write anything.
                // Actually we should probably delete the old files.
                if let EntityMeta::Parallel(_) = current_meta {
                    let _ = xfs_ext::remove_file(&mut *fs, &self.dot_metadata_path());
                } else if let EntityMeta::Inside(_) = current_meta {
                    let _ = xfs_ext::remove_file(&mut *fs, &self.slash_metadata_path());
                }
            }
            EntityMeta::Parallel(m) => {
                let toml_str = toml::to_string(&m.value)?;
                let path = self.dot_metadata_path();
                if let Some(parent) = path.parent() {
                    fs.create_dir_all(parent)?;
                }
                fs.writer(&path)?.write_all(toml_str.as_bytes())?;
            }
            EntityMeta::Inside(m) => {
                let toml_str = toml::to_string(&m.value)?;
                let path = self.slash_metadata_path();
                fs.create_dir_all(path.parent().unwrap())?;
                fs.writer(&path)?.write_all(toml_str.as_bytes())?;
            }
            EntityMeta::InHeader(m, sep) => {
                let content_body = current_content.content().unwrap_or("");
                let mut to_write = String::new();
                to_write.push_str("```toml\n");
                to_write.push_str(&toml::to_string(&m.value)?);
                to_write.push_str("```\n");
                if let Some(s) = sep {
                    to_write.push_str(&s);
                } else if !content_body.starts_with('\n') {
                    to_write.push_str("\n");
                }
                to_write.push_str(content_body);

                let path = match current_content {
                    EntityContent::Parallel(_) => self.dot_content_path(),
                    EntityContent::Inside(_) => self.slash_content_path(),
                    EntityContent::None => {
                        // If no content exists, we need to create a content file
                        if self.path.entries.is_empty() {
                            self.slash_content_path()
                        } else {
                            self.dot_content_path()
                        }
                    }
                };
                if let Some(parent) = path.parent() {
                    fs.create_dir_all(parent)?;
                }
                fs.writer(&path)?.write_all(to_write.as_bytes())?;
            }
        }
        Ok(())
    }

    pub fn delete(&self) -> anyhow::Result<()> {
        let mut fs = self.fs.borrow_mut();

        // 1. Delete content files
        let _ = xfs_ext::remove_file(&mut *fs, &self.dot_content_path());
        let _ = xfs_ext::remove_file(&mut *fs, &self.slash_content_path());

        // 2. Delete metadata files
        let _ = xfs_ext::remove_file(&mut *fs, &self.dot_metadata_path());
        let _ = xfs_ext::remove_file(&mut *fs, &self.slash_metadata_path());

        // 3. Delete the directory if it exists and is empty?
        // Actually, Dot children of this entity also need to be deleted.
        // And if it's a Slash entity, its directory should be removed.
        let on_disk = self.on_disk_path();
        if fs.is_dir(&on_disk) {
            xfs_ext::remove_dir_all(&mut *fs, &on_disk)?;
        }

        // Handle Dot children (which are files/dirs in the parent directory)
        if !self.path.entries.is_empty() {
            let p = self.on_disk_path();
            let p_str = p.to_str().unwrap();
            let p_dot_str = format!("{}.", p_str);
            let parent_dir = p.parent().unwrap();

            // We need to find all files starting with "parent.name."
            // This is a bit expensive as we have to list the parent directory.
            let mut to_delete = vec![];
            for de in fs.read_dir(parent_dir)? {
                let de = de?;
                let path = de.path();
                let path_str = path.to_str().unwrap();
                if path_str.starts_with(&p_dot_str) {
                    to_delete.push(path);
                }
            }
            for path in to_delete {
                if fs.is_dir(&path) {
                    xfs_ext::remove_dir_all(&mut *fs, &path)?;
                } else {
                    xfs_ext::remove_file(&mut *fs, &path)?;
                }
            }
        }

        Ok(())
    }

    pub fn move_to(&mut self, new_path: EntityPath) -> anyhow::Result<()> {
        let old_on_disk = self.on_disk_path();
        let new_on_disk = new_path.to_pathbuf(&self.base_path);

        let mut fs = self.fs.borrow_mut();

        // 1. Move the main files/directory
        // We need to check what actually exists.
        let mut moved_anything = false;

        let dot_content = self.dot_content_path();
        if fs.is_file(&dot_content) {
            let new_dot_content = new_path.to_pathbuf(&self.base_path).with_added_extension("md");
            if let Some(parent) = new_dot_content.parent() {
                fs.create_dir_all(parent)?;
            }
            xfs_ext::rename(&mut *fs, &dot_content, &new_dot_content)?;
            moved_anything = true;
        }

        let dot_metadata = self.dot_metadata_path();
        if fs.is_file(&dot_metadata) {
            let new_dot_metadata = new_path.to_pathbuf(&self.base_path).with_extension("meta.toml");
            if let Some(parent) = new_dot_metadata.parent() {
                fs.create_dir_all(parent)?;
            }
            xfs_ext::rename(&mut *fs, &dot_metadata, &new_dot_metadata)?;
            moved_anything = true;
        }

        if fs.is_dir(&old_on_disk) {
            if let Some(parent) = new_on_disk.parent() {
                fs.create_dir_all(parent)?;
            }
            xfs_ext::rename(&mut *fs, &old_on_disk, &new_on_disk)?;
            moved_anything = true;
        }

        // 2. Move Dot children
        if !self.path.entries.is_empty() {
            let p = self.on_disk_path();
            let p_str = p.to_str().unwrap();
            let p_dot_str = format!("{}.", p_str);
            let parent_dir = p.parent().unwrap();

            let new_p = new_path.to_pathbuf(&self.base_path);
            let new_p_str = new_p.to_str().unwrap();
            let new_p_dot_str = format!("{}.", new_p_str);

            let mut to_move = vec![];
            for de in fs.read_dir(parent_dir)? {
                let de = de?;
                let path = de.path();
                let path_str = path.to_str().unwrap();
                if path_str.starts_with(&p_dot_str) {
                    let suffix = &path_str[p_dot_str.len()..];
                    let new_child_path = PathBuf::from(format!("{}{}", new_p_dot_str, suffix));
                    to_move.push((path, new_child_path));
                }
            }
            for (old_child, new_child) in to_move {
                if let Some(parent) = new_child.parent() {
                    fs.create_dir_all(parent)?;
                }
                xfs_ext::rename(&mut *fs, &old_child, &new_child)?;
                moved_anything = true;
            }
        }

        if !moved_anything {
            bail!("Nothing found to move at {:?}", old_on_disk);
        }

        self.path = new_path;
        Ok(())
    }

    pub fn create_child(
        &self,
        entry: EntityPathEntry,
        node_type: String,
    ) -> anyhow::Result<LiveEntity> {
        let child_path = self.path.extend(entry.clone());
        let child_handle = LiveEntity::new(
            self.fs.clone(),
            self.base_path.clone(),
            child_path,
            node_type,
            self.schema.clone(),
        );

        // Ensure parent directory exists if it's a Slash entry
        if let EntityPathEntry::Slash(_) = entry {
            let mut fs = self.fs.borrow_mut();
            fs.create_dir_all(&self.on_disk_path())?;
        }

        Ok(child_handle)
    }
}

#[cfg(test)]
mod common {
    use super::*;
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
                    multiple: true,
                }],
                allow_additional: false,
            },
        );
        loader.schema.add_entity_type(
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

        loader.schema.add_entity_type(
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
        loader.schema.add_entity_type(
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
        loader.schema.add_entity_type(
            EntityTypeDescription {
                name: "Scene".to_string(),
                children: vec![],
                allow_additional: true,
            },
        );
        loader.schema.add_entity_type(
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

    fn check_header_meta(
        meta: &EntityMeta,
        key: &str,
        expected_val: &str,
        expected_sep: Option<&str>,
    ) {
        if let EntityMeta::InHeader(m, sep) = meta {
            assert_eq!(m.value.get(key).unwrap().as_str().unwrap(), expected_val);
            assert_eq!(sep.as_deref(), expected_sep);
        } else {
            panic!("Expected InHeader metadata, got {:?}", meta);
        }
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

    #[test]
    fn test_load_entity_conflict_header_and_meta_toml() {
        let content = "```toml\nfoo = \"bar\"\n```\nActual content";
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", content);
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "other = \"meta\"\n");

        let loader = dummy_loader();
        let entity_path = EntityPath::empty().extend_slash("entity1");
        let result = loader.try_load_entity(&fs, &PathBuf::from("foo"), &entity_path, "TestType");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Multiple metadata sources"));
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
        let file_content_raw = &fs
            .resolve_path(&PathBuf::from("bar/entity1.md"))
            .unwrap()
            .as_file()
            .unwrap()
            .contents;
        let binding = file_content_raw.borrow();
        let file_content = std::str::from_utf8(binding.as_slice()).unwrap();
        assert_eq!(file_content, content);
    }

    #[test]
    #[should_panic(expected = "rename not yet in Xfs trait")]
    fn test_live_entity_move() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", "Content");
        let fs = Rc::new(RefCell::new(fs));

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "TestType".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Rc::new(loader.schema);

        let mut live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "TestType".to_string(),
            schema,
        );

        let new_path = EntityPath::empty().extend_slash("entity2");
        live.move_to(new_path).unwrap();
    }

    #[test]
    fn test_live_entity_write() {
        let fs = mockfs::MockFS::new();
        let fs = Rc::new(RefCell::new(fs));

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "TestType".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "TestType".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: true,
        });
        let schema = Rc::new(loader.schema);

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "TestType".to_string(),
            schema,
        );

        // 1. set_content
        live.set_content("New content").unwrap();
        assert_eq!(live.content().unwrap(), EntityContent::parallel("New content"));

        // 2. set_metadata (Inside)
        let meta = Metadata { value: toml::from_str("a = 1").unwrap() };
        live.set_metadata(EntityMeta::Inside(meta.clone())).unwrap();
        assert_eq!(live.metadata().unwrap(), EntityMeta::Inside(meta));

        // 3. create_child
        let child = live.create_child(EntityPathEntry::Dot("child1".to_string()), "TestType".to_string()).unwrap();
        child.set_content("Child content").unwrap();
        assert_eq!(live.children().unwrap().len(), 1);

        // 4. delete
        // child.delete().unwrap();
        // assert_eq!(live.children().unwrap().len(), 0);
    }

    #[test]
    fn test_live_entity_navigation() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Root");
        create_file_with_content(&mut fs, "foo/entity1/child1", "content.md", "Child");
        create_file_with_content(&mut fs, "foo/entity1/child1/grandchild1", "content.md", "Grandchild");
        let fs = Rc::new(RefCell::new(fs));

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "Type".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "Type".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: true,
        });
        let schema = Rc::new(loader.schema);

        let root = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        let children = root.children().unwrap();
        assert_eq!(children.len(), 1);
        let child = &children[0];
        assert_eq!(child.content().unwrap().content().unwrap(), "Child");

        let grandchildren = child.children().unwrap();
        assert_eq!(grandchildren.len(), 1);
        assert_eq!(grandchildren[0].content().unwrap().content().unwrap(), "Grandchild");
    }

    #[test]
    fn test_live_entity_read() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "```toml\nkey = \"val\"\n```\n---\nHello");
        create_file_with_content(&mut fs, "foo/entity1", "child1.md", "Child content");
        let fs = Rc::new(RefCell::new(fs));

        let mut loader = EntityLoader::new();
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "TestType".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "child1".to_string(),
                node_type: "ChildType".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        loader.schema.add_entity_type(EntityTypeDescription {
            name: "ChildType".to_string(),
            children: vec![],
            allow_additional: false,
        });
        let schema = Rc::new(loader.schema);

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "TestType".to_string(),
            schema,
        );

        assert_eq!(live.content().unwrap(), EntityContent::inside("Hello"));
        let meta = live.metadata().unwrap();
        if let EntityMeta::InHeader(m, sep) = meta {
            assert_eq!(m.value.get("key").unwrap().as_str().unwrap(), "val");
            assert_eq!(sep.unwrap(), "---\n");
        } else {
            panic!("Expected InHeader metadata");
        }

        let children = live.children().unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].path(), &EntityPath::empty().extend_slash("entity1").extend_slash("child1"));
        assert_eq!(children[0].content().unwrap(), EntityContent::parallel("Child content"));
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
