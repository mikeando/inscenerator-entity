use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use anyhow::{bail};
use inscenerator_xfs::Xfs;

use crate::entity::{EntityPath, EntityPathEntry, EntityContent, EntityMeta, utils};
use crate::schema::{Schema};

/// Stubs for missing Xfs functionality.
pub mod xfs_ext {
    use super::*;
    /// Stub for removing a file.
    pub fn remove_file(_fs: &mut dyn Xfs, _path: &Path) -> anyhow::Result<()> {
        todo!("remove_file not yet in Xfs trait")
    }
    /// Stub for removing a directory and all its contents.
    pub fn remove_dir_all(_fs: &mut dyn Xfs, _path: &Path) -> anyhow::Result<()> {
        todo!("remove_dir_all not yet in Xfs trait")
    }
    /// Stub for renaming a file or directory.
    pub fn rename(_fs: &mut dyn Xfs, _from: &Path, _to: &Path) -> anyhow::Result<()> {
        todo!("rename not yet in Xfs trait")
    }
}

/// Shared context for a tree of LiveEntities.
pub struct LiveEntityRoot {
    /// The underlying filesystem.
    pub fs: Rc<RefCell<dyn Xfs>>,
    /// The base path on disk for this entity tree.
    pub base_path: PathBuf,
    /// The schema defining entity types and rules.
    pub schema: Rc<Schema>,
}

impl fmt::Debug for LiveEntityRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LiveEntityRoot")
            .field("base_path", &self.base_path)
            .finish()
    }
}

/// A handle to an entity that provides on-demand read and write access to the filesystem.
#[derive(Clone, Debug)]
pub struct LiveEntity {
    /// Shared root context.
    pub root: Rc<LiveEntityRoot>,
    /// Logical path of the entity.
    pub path: EntityPath,
    /// Type name of the entity.
    pub node_type: String,
}

impl LiveEntity {
    /// Creates a new LiveEntity handle.
    pub fn new(
        fs: Rc<RefCell<dyn Xfs>>,
        base_path: PathBuf,
        path: EntityPath,
        node_type: String,
        schema: Rc<Schema>,
    ) -> Self {
        Self {
            root: Rc::new(LiveEntityRoot {
                fs,
                base_path,
                schema,
            }),
            path,
            node_type,
        }
    }

    /// Returns the logical path of this entity.
    pub fn path(&self) -> &EntityPath {
        &self.path
    }

    /// Returns the type name of this entity.
    pub fn node_type(&self) -> &str {
        &self.node_type
    }

    fn on_disk_path(&self) -> PathBuf {
        self.path.to_pathbuf(&self.root.base_path)
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

    fn get_content_info(&self) -> anyhow::Result<(Option<String>, bool, PathBuf)> {
        let fs = self.root.fs.borrow();
        let is_root = self.path.entries.is_empty();

        let dot_content_file = self.dot_content_path();
        let slash_content_file = self.slash_content_path();

        if is_root {
            let content = utils::try_load_file_as_string(&*fs, &slash_content_file)?;
            return Ok((content, false, slash_content_file));
        }

        if let Some(c) = utils::try_load_file_as_string(&*fs, &dot_content_file)? {
            if fs.is_file(&slash_content_file) {
                bail!(
                    "Both {} and {} exist.",
                    dot_content_file.display(),
                    slash_content_file.display()
                );
            }
            return Ok((Some(c), true, dot_content_file));
        }

        let content = utils::try_load_file_as_string(&*fs, &slash_content_file)?;
        Ok((content, false, slash_content_file))
    }

    /// Reads the content of the entity from disk.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if storage format is inconsistent.
    pub fn content(&self) -> anyhow::Result<EntityContent> {
        let (content_str, is_parallel, _) = self.get_content_info()?;

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

    /// Reads the metadata of the entity from disk.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if multiple metadata sources are found.
    pub fn metadata(&self) -> anyhow::Result<EntityMeta> {
        let fs = self.root.fs.borrow();
        let is_root = self.path.entries.is_empty();

        let (content_str, _, _) = self.get_content_info()?;

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

    /// Returns handles to the children of this entity as defined by the schema.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if an unexpected child is encountered.
    pub fn children(&self) -> anyhow::Result<Vec<LiveEntity>> {
        let fs = self.root.fs.borrow();
        let is_root = self.path.entries.is_empty();

        let dot_children = if !is_root {
            utils::find_dot_children(&*fs, &self.root.base_path, &self.path)?
        } else {
            vec![]
        };
        let slash_children = utils::find_slash_children(&*fs, &self.root.base_path, &self.path)?;
        let children_paths = dot_children
            .into_iter()
            .chain(slash_children)
            .collect::<Vec<EntityPath>>();

        let entity_type_descriptor = self.root.schema.get_entity_type(&self.node_type)?;

        let mut loaded_children = vec![];
        for child_path in children_paths {
            let child_name: &str = child_path.last_name().unwrap();
            let mut found_match = false;
            for child_rule in &entity_type_descriptor.children {
                let re = regex::Regex::new(&child_rule.name_regex).unwrap();
                if re.is_match(child_name) {
                    loaded_children.push(LiveEntity {
                        root: self.root.clone(),
                        path: child_path.clone(),
                        node_type: child_rule.node_type.clone(),
                    });
                    found_match = true;
                    break;
                }
            }
            if !found_match && !entity_type_descriptor.allow_additional {
                bail!(
                    "Unexpected child entity '{}' in entity '{:?}'",
                    child_name,
                    self.on_disk_path()
                );
            }
        }

        Ok(loaded_children)
    }

    /// Updates the content of the entity on disk.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn set_content(&self, new_content: &str) -> anyhow::Result<()> {
        let current_meta = self.metadata()?;
        let (content_str, _is_parallel, path) = self.get_content_info()?;

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

        let final_path = if content_str.is_none() {
             let fs = self.root.fs.borrow();
             if self.path.entries.is_empty() || fs.is_dir(&self.on_disk_path()) {
                self.slash_content_path()
             } else {
                self.dot_content_path()
             }
        } else {
            path
        };

        let mut fs = self.root.fs.borrow_mut();
        if let Some(parent) = final_path.parent() {
            fs.create_dir_all(parent)?;
        }
        let mut writer = fs.writer(&final_path)?;
        writer.write_all(to_write.as_bytes())?;
        Ok(())
    }

    /// Updates the metadata of the entity on disk.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn set_metadata(&self, meta: EntityMeta) -> anyhow::Result<()> {
        let current_meta = self.metadata()?;
        let current_content = self.content()?;

        let mut fs = self.root.fs.borrow_mut();

        if let EntityMeta::InHeader(_, _) = current_meta {
            if !matches!(meta, EntityMeta::InHeader(_, _)) {
                let (content_str, _, path) = self.get_content_info()?;
                if let Some(c) = content_str {
                     let (_, _, a): (_, _, String) = utils::parse_header(&c).unwrap();
                     let mut writer = fs.writer(&path)?;
                     writer.write_all(a.as_bytes())?;
                }
            }
        }

        match meta {
            EntityMeta::None => {
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

                let (_, _, path) = self.get_content_info()?;
                let final_path = if current_content.is_none() {
                     if self.path.entries.is_empty() {
                         self.slash_content_path()
                     } else {
                         self.dot_content_path()
                     }
                } else {
                    path
                };

                if let Some(parent) = final_path.parent() {
                    fs.create_dir_all(parent)?;
                }
                fs.writer(&final_path)?.write_all(to_write.as_bytes())?;
            }
        }
        Ok(())
    }

    /// Deletes the entity and its associated files from disk.
    ///
    /// If `recursive` is true, all children (both Slash and Dot types) are deleted.
    /// If `recursive` is false and the entity has children, deletion will fail.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if entity is not empty and recursive=false.
    pub fn delete(&self, recursive: bool) -> anyhow::Result<()> {
        let mut fs = self.root.fs.borrow_mut();

        if !recursive {
            let children = utils::find_dot_children(&*fs, &self.root.base_path, &self.path)?;
            let slash_children = utils::find_slash_children(&*fs, &self.root.base_path, &self.path)?;
            if !children.is_empty() || !slash_children.is_empty() {
                bail!("Entity is not empty and recursive delete not requested");
            }
        }

        // 1. Delete content files
        let _ = xfs_ext::remove_file(&mut *fs, &self.dot_content_path());
        let _ = xfs_ext::remove_file(&mut *fs, &self.slash_content_path());

        // 2. Delete metadata files
        let _ = xfs_ext::remove_file(&mut *fs, &self.dot_metadata_path());
        let _ = xfs_ext::remove_file(&mut *fs, &self.slash_metadata_path());

        // 3. Delete the directory if it exists
        let on_disk = self.on_disk_path();
        if fs.is_dir(&on_disk) {
            xfs_ext::remove_dir_all(&mut *fs, &on_disk)?;
        }

        // Handle Dot children
        if self.path.entries.is_empty() {
            return Ok(());
        }

        let p = self.on_disk_path();
        let p_str = p.to_str().unwrap();
        let p_dot_str = format!("{}.", p_str);
        let parent_dir = p.parent().unwrap();

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

        Ok(())
    }

    /// Moves/renames the entity on disk to a new logical path.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if nothing is found to move.
    pub fn move_to(&mut self, new_path: EntityPath) -> anyhow::Result<()> {
        let old_on_disk = self.on_disk_path();
        let new_on_disk = new_path.to_pathbuf(&self.root.base_path);

        let mut fs = self.root.fs.borrow_mut();

        let mut moved_anything = false;

        let dot_content = self.dot_content_path();
        if fs.is_file(&dot_content) {
            let new_dot_content = new_path.to_pathbuf(&self.root.base_path).with_added_extension("md");
            if let Some(parent) = new_dot_content.parent() {
                fs.create_dir_all(parent)?;
            }
            xfs_ext::rename(&mut *fs, &dot_content, &new_dot_content)?;
            moved_anything = true;
        }

        let dot_metadata = self.dot_metadata_path();
        if fs.is_file(&dot_metadata) {
            let new_dot_metadata = new_path.to_pathbuf(&self.root.base_path).with_extension("meta.toml");
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

        if self.path.entries.is_empty() {
            if !moved_anything {
                bail!("Nothing found to move at {:?}", old_on_disk);
            }
            self.path = new_path;
            return Ok(());
        }

        let p = self.on_disk_path();
        let p_str = p.to_str().unwrap();
        let p_dot_str = format!("{}.", p_str);
        let parent_dir = p.parent().unwrap();

        let new_p = new_path.to_pathbuf(&self.root.base_path);
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

        if !moved_anything {
            bail!("Nothing found to move at {:?}", old_on_disk);
        }

        self.path = new_path;
        Ok(())
    }

    /// Creates a new child entity handle.
    ///
    /// Note: This does not write anything to disk immediately unless it is a Slash entry,
    /// in which case the parent directory is created.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn create_child(
        &self,
        entry: EntityPathEntry,
        node_type: String,
    ) -> anyhow::Result<LiveEntity> {
        let child_path = self.path.extend(entry.clone());
        let child_handle = LiveEntity {
            root: self.root.clone(),
            path: child_path,
            node_type,
        };

        if let EntityPathEntry::Slash(_) = entry {
            let mut fs = self.root.fs.borrow_mut();
            fs.create_dir_all(&self.on_disk_path())?;
        }

        Ok(child_handle)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inscenerator_xfs::mockfs;
    use crate::schema::ChildEntityRules;
    use crate::schema::EntityTypeDescription;

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

    fn setup_schema() -> Rc<Schema> {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Type".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "Type".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: true,
        });
        Rc::new(schema)
    }

    #[test]
    fn test_live_entity_read() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "```toml\nkey = \"val\"\n```\n---\nHello");
        create_file_with_content(&mut fs, "foo/entity1", "child1.md", "Child content");
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
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
    fn test_live_entity_write() {
        let fs = mockfs::MockFS::new();
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        // 1. set_content
        live.set_content("New content").unwrap();
        assert_eq!(live.content().unwrap(), EntityContent::parallel("New content"));

        // 2. set_metadata (Inside)
        let meta = crate::entity::Metadata { value: toml::from_str("a = 1").unwrap() };
        live.set_metadata(EntityMeta::Inside(meta.clone())).unwrap();
        assert_eq!(live.metadata().unwrap(), EntityMeta::Inside(meta));

        // 3. create_child
        let child = live.create_child(EntityPathEntry::Dot("child1".to_string()), "Type".to_string()).unwrap();
        child.set_content("Child content").unwrap();
        assert_eq!(live.children().unwrap().len(), 1);
    }

    #[test]
    fn test_conflict_detection() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Inside");
        create_file_with_content(&mut fs, "foo", "entity1.md", "Parallel");
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        assert!(live.content().is_err());
    }

    #[test]
    #[should_panic(expected = "remove_file not yet in Xfs trait")]
    fn test_metadata_cleanup() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "a = 1");
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        live.set_metadata(EntityMeta::None).unwrap();
    }

    #[test]
    #[should_panic(expected = "remove_file not yet in Xfs trait")]
    fn test_live_entity_delete() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Hello");
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        live.delete(true).unwrap();
    }

    #[test]
    #[should_panic(expected = "rename not yet in Xfs trait")]
    fn test_live_entity_move() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", "Content");
        let fs = Rc::new(RefCell::new(fs));
        let schema = setup_schema();

        let mut live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        let new_path = EntityPath::empty().extend_slash("entity2");
        live.move_to(new_path).unwrap();
    }
}
