use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::fmt;

use anyhow::{bail};
use inscenerator_xfs::Xfs;

use std::io::Write;

use crate::entity::{EntityPath, EntityPathEntry, EntityContent, EntityMeta, Metadata, utils};
use crate::schema::{Schema};

/// Shared context for a tree of LiveEntities.
pub struct LiveEntityRoot {
    /// The underlying filesystem.
    pub fs: Arc<Mutex<dyn Xfs + Send + Sync>>,
    /// The base path on disk for this entity tree.
    pub base_path: PathBuf,
    /// The schema defining entity types and rules.
    pub schema: Arc<Schema>,
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
    pub root: Arc<LiveEntityRoot>,
    /// Logical path of the entity.
    pub path: EntityPath,
    /// Type name of the entity.
    pub node_type: String,
}

/// Controls where content is written relative to the entity's disk path.
#[derive(Debug, Clone)]
enum ChildContentLayout {
    /// Layout is chosen automatically: Slash entries use Inside, Dot entries use Parallel.
    Inferred,
    /// Content is written inside the entity's directory (`dir/content.md`).
    Inside,
    /// Content is written alongside the entity (`name.md`).
    Parallel,
}

/// Builder for creating a new child entity on disk.
///
/// Obtain via [`LiveEntity::create_child`]. Call [`build`](ChildBuilder::build) to write to disk.
#[derive(Debug, Clone)]
pub struct ChildBuilder {
    root: Arc<LiveEntityRoot>,
    /// Logical path of the parent entity.
    parent_path: EntityPath,
    /// Node type of the parent (may be "Auto", resolved via actual_type() at build time).
    parent_node_type: String,
    entry: EntityPathEntry,
    node_type_override: Option<String>,
    content_text: Option<String>,
    content_layout: ChildContentLayout,
    metadata: Option<EntityMeta>,
    nested_children: Vec<ChildBuilder>,
}

impl ChildBuilder {
    /// Overrides the node type inferred from the schema.
    ///
    /// Required when the schema slot is `"Auto"` or the parent has `allow_additional = true`.
    /// If the schema infers a concrete type, the override must match it exactly.
    /// Last call wins.
    pub fn with_type(mut self, node_type: &str) -> Self {
        self.node_type_override = Some(node_type.to_string());
        self
    }

    /// Sets the content text, inferring layout from entry type.
    ///
    /// Slash entries default to Inside (`dir/content.md`);
    /// Dot entries default to Parallel (`name.md`).
    /// Last call wins.
    pub fn with_content(mut self, text: &str) -> Self {
        self.content_text = Some(text.to_string());
        self.content_layout = ChildContentLayout::Inferred;
        self
    }

    /// Sets the content text and forces Inside layout (`dir/content.md`).
    ///
    /// Last call wins.
    pub fn with_content_inside(mut self, text: &str) -> Self {
        self.content_text = Some(text.to_string());
        self.content_layout = ChildContentLayout::Inside;
        self
    }

    /// Sets the content text and forces Parallel layout (`name.md`).
    ///
    /// Last call wins.
    pub fn with_content_parallel(mut self, text: &str) -> Self {
        self.content_text = Some(text.to_string());
        self.content_layout = ChildContentLayout::Parallel;
        self
    }

    /// Sets metadata from an [`EntityMeta`] value.
    ///
    /// The layout (Inside, Parallel, InHeader) is taken from the variant.
    /// `EntityMeta::None` clears any previously set metadata.
    /// Last call wins.
    pub fn with_metadata(mut self, meta: EntityMeta) -> Self {
        if matches!(meta, EntityMeta::None) {
            self.metadata = None;
        } else {
            self.metadata = Some(meta);
        }
        self
    }

    /// Sets metadata to be written inside the entity directory (`dir/meta.toml`).
    ///
    /// Last call wins.
    pub fn with_metadata_inside(mut self, meta: Metadata) -> Self {
        self.metadata = Some(EntityMeta::Inside(meta));
        self
    }

    /// Sets metadata to be written alongside the entity (`name.meta.toml`).
    ///
    /// Last call wins.
    pub fn with_metadata_parallel(mut self, meta: Metadata) -> Self {
        self.metadata = Some(EntityMeta::Parallel(meta));
        self
    }

    /// Adds a nested child builder.
    ///
    /// The closure receives a fresh [`ChildBuilder`] whose parent path is set to this
    /// entity's path. Configure it inside the closure and return the result.
    /// Nested children are built (in order) when [`build`](Self::build) is called.
    pub fn with_child<F>(mut self, entry: EntityPathEntry, f: F) -> Self
    where
        F: FnOnce(ChildBuilder) -> ChildBuilder,
    {
        let own_path = self.parent_path.extend(self.entry.clone());
        let inner = ChildBuilder {
            root: self.root.clone(),
            parent_path: own_path,
            parent_node_type: String::new(), // intentionally unused: nested builders
                                             // always enter via build_internal(parent_type),
                                             // never via build() which reads this field
            entry,
            node_type_override: None,
            content_text: None,
            content_layout: ChildContentLayout::Inferred,
            metadata: None,
            nested_children: vec![],
        };
        self.nested_children.push(f(inner));
        self
    }

    /// Validates configuration, writes the child entity to disk, and returns a handle to it.
    ///
    /// For Slash entries this always creates a directory. For Dot entries at least
    /// one of content, metadata, or nested children must be provided.
    ///
    /// # Errors
    ///
    /// Returns an error if the child already exists, if the schema rejects the child name
    /// or type, if InHeader metadata is set without content, or if disk access fails.
    pub fn build(self) -> anyhow::Result<LiveEntity> {
        let parent_live = LiveEntity {
            root: self.root.clone(),
            path: self.parent_path.clone(),
            node_type: self.parent_node_type.clone(),
        };
        let parent_type = parent_live.actual_type()?;
        self.build_internal(&parent_type)
    }

    fn build_internal(mut self, parent_type: &str) -> anyhow::Result<LiveEntity> {
        // --- Type resolution ---
        let child_name = match &self.entry {
            EntityPathEntry::Slash(n) | EntityPathEntry::Dot(n) => n.as_str(),
        };

        let entity_type_descriptor = self.root.schema.get_entity_type(parent_type)?;

        let inferred_type: Option<String> = entity_type_descriptor
            .children
            .iter()
            .find(|rule| {
                regex::Regex::new(&rule.name_regex)
                    .map(|re| re.is_match(child_name))
                    .unwrap_or(false)
            })
            .map(|rule| rule.node_type.clone());

        let resolved_type = match (&inferred_type, &self.node_type_override) {
            // Rule matched, no override
            (Some(inferred), None) => inferred.clone(),
            // Rule matched, override matches
            (Some(inferred), Some(override_type)) if inferred == override_type => inferred.clone(),
            // Rule matched "Auto", override provides concrete type
            (Some(inferred), Some(override_type)) if inferred == "Auto" => override_type.clone(),
            // Rule matched concrete type, override differs => error
            (Some(inferred), Some(override_type)) => {
                bail!(
                    "Type mismatch: schema inferred '{}' but with_type specified '{}'",
                    inferred, override_type
                );
            }
            // No rule matched, allow_additional = true, override provided
            (None, Some(override_type)) if entity_type_descriptor.allow_additional => {
                override_type.clone()
            }
            // No rule matched, allow_additional = true, no override => error
            (None, None) if entity_type_descriptor.allow_additional => {
                bail!(
                    "Child '{}' does not match any schema rule; call with_type() to specify its type",
                    child_name
                );
            }
            // No rule matched, allow_additional = false => error
            (None, _) => {
                bail!("Unexpected child '{}' in entity of type '{}'", child_name, parent_type);
            }
        };

        // If the schema rule explicitly sets node_type = "Auto" but no with_type() was called,
        // we cannot write a meaningful type — error eagerly rather than deferring to the next read.
        if resolved_type == "Auto" && self.node_type_override.is_none() {
            let child_name = match &self.entry {
                EntityPathEntry::Slash(n) | EntityPathEntry::Dot(n) => n.as_str(),
            };
            bail!(
                "Child '{}' has schema type 'Auto' — call with_type() to specify the concrete type",
                child_name
            );
        }

        // Determine whether this is an Auto-override case
        let is_auto_override = inferred_type.as_deref() == Some("Auto")
            || (inferred_type.is_none() && entity_type_descriptor.allow_additional);

        // For Auto-override with metadata provided, validate it's a Table
        if is_auto_override {
            if let Some(ref meta) = self.metadata {
                let meta_value = match meta {
                    EntityMeta::Inside(m) | EntityMeta::Parallel(m) => Some(&m.value),
                    EntityMeta::InHeader(m, _, _) => Some(&m.value),
                    EntityMeta::None => None,
                };
                if let Some(v) = meta_value {
                    if !v.is_table() {
                        bail!("Metadata value must be a TOML table to merge 'type' key");
                    }
                }
            }
        }

        // Merge type key into existing metadata for Auto-override
        if is_auto_override && self.metadata.is_some() {
            match &mut self.metadata {
                Some(EntityMeta::Inside(m)) | Some(EntityMeta::Parallel(m)) => {
                    if let toml::Value::Table(ref mut table) = m.value {
                        table.insert(
                            "type".to_string(),
                            toml::Value::String(resolved_type.clone()),
                        );
                    }
                    // (Table check already done above — if not a Table, we already bailed)
                }
                Some(EntityMeta::InHeader(m, _, _)) => {
                    if let toml::Value::Table(ref mut table) = m.value {
                        table.insert(
                            "type".to_string(),
                            toml::Value::String(resolved_type.clone()),
                        );
                    }
                }
                _ => {}
            }
        }

        // Root entity may only have Slash children
        if self.parent_path.entries.is_empty() {
            if let EntityPathEntry::Dot(_) = &self.entry {
                bail!("Root entities may only have Slash children");
            }
        }

        let own_path = self.parent_path.extend(self.entry.clone());
        let own_disk_path = own_path.to_pathbuf(&self.root.base_path);

        match &self.entry {
            EntityPathEntry::Slash(_) => {
                // Existence check
                {
                    let fs = self.root.fs.lock().unwrap();
                    if fs.is_dir(&own_disk_path) {
                        bail!("Child already exists at {:?}", own_disk_path);
                    }
                }
                // Create directory
                self.root.fs.lock().unwrap().create_dir_all(&own_disk_path)?;
            }
            EntityPathEntry::Dot(n) => {
                // Dot child must have content, metadata, or nested children
                if self.content_text.is_none()
                    && self.metadata.is_none()
                    && self.nested_children.is_empty()
                {
                    bail!("Dot child '{}' has nothing to write to disk; provide content, metadata, or children", n);
                }

                // Existence check: .md or .meta.toml files
                {
                    let fs_guard = self.root.fs.lock().unwrap();
                    if fs_guard.is_file(&own_disk_path.with_added_extension("md"))
                        || fs_guard.is_file(&own_disk_path.with_added_extension("meta.toml"))
                    {
                        bail!("Dot child '{}' already exists at {:?}", n, own_disk_path);
                    }
                    // Also check for any file in the parent dir that starts with "parent.notes."
                    let check_dir = own_disk_path.parent().unwrap();
                    let own_name = own_disk_path.file_name().unwrap().to_str().unwrap();
                    let dot_prefix = format!("{}.", own_name);
                    if let Ok(entries) = fs_guard.read_dir(check_dir) {
                        for entry in entries.flatten() {
                            if let Some(fname_str) = entry.path().file_name().and_then(|f| f.to_str()) {
                                if fname_str.starts_with(&dot_prefix) {
                                    bail!("Dot child '{}' already exists (found {:?})", n, entry.path());
                                }
                            }
                        }
                    }
                }
            }
        }

        // InHeader metadata requires content
        if let Some(EntityMeta::InHeader(_, _, _)) = &self.metadata {
            if self.content_text.is_none() {
                bail!("InHeader metadata requires content to be set via with_content()");
            }
        }

        // Write type to metadata for Auto-override (minimal meta.toml when no user metadata provided)
        let auto_type_written = is_auto_override && self.metadata.is_none();
        if auto_type_written {
            let meta_path = match &self.entry {
                EntityPathEntry::Slash(_) => own_disk_path.join("meta.toml"),
                EntityPathEntry::Dot(_) => own_disk_path.with_added_extension("meta.toml"),
            };
            let content = format!("type = \"{}\"\n", resolved_type);
            let mut fs = self.root.fs.lock().unwrap();
            if let Some(parent) = meta_path.parent() {
                fs.create_dir_all(parent)?;
            }
            fs.writer(&meta_path)?.write_all(content.as_bytes())?;
        }

        let return_node_type = if is_auto_override {
            "Auto".to_string()
        } else {
            resolved_type.clone()
        };

        // Compute the content path (needed for both plain content write and InHeader)
        let content_path = match self.content_layout {
            ChildContentLayout::Inside => own_disk_path.join("content.md"),
            ChildContentLayout::Parallel => own_disk_path.with_added_extension("md"),
            ChildContentLayout::Inferred => match &self.entry {
                EntityPathEntry::Slash(_) => own_disk_path.join("content.md"),
                EntityPathEntry::Dot(_) => own_disk_path.with_added_extension("md"),
            },
        };

        // --- Content write ---
        // Skip plain content write when InHeader is used (written together with header below)
        let is_inheader = matches!(&self.metadata, Some(EntityMeta::InHeader(_, _, _)));
        if let Some(ref text) = self.content_text {
            if !is_inheader {
                let mut fs = self.root.fs.lock().unwrap();
                if let Some(parent) = content_path.parent() {
                    fs.create_dir_all(parent)?;
                }
                fs.writer(&content_path)?.write_all(text.as_bytes())?;
            }
        }

        // --- Metadata write ---
        if !auto_type_written {
            if let Some(ref meta) = self.metadata {
                match meta {
                    EntityMeta::Inside(m) => {
                        let path = own_disk_path.join("meta.toml");
                        let toml_str = toml::to_string(&m.value)?;
                        let mut fs = self.root.fs.lock().unwrap();
                        if let Some(p) = path.parent() {
                            fs.create_dir_all(p)?;
                        }
                        fs.writer(&path)?.write_all(toml_str.as_bytes())?;
                    }
                    EntityMeta::Parallel(m) => {
                        let path = own_disk_path.with_added_extension("meta.toml");
                        let toml_str = toml::to_string(&m.value)?;
                        let mut fs = self.root.fs.lock().unwrap();
                        if let Some(p) = path.parent() {
                            fs.create_dir_all(p)?;
                        }
                        fs.writer(&path)?.write_all(toml_str.as_bytes())?;
                    }
                    EntityMeta::InHeader(m, sep, header_type) => {
                        // Content is guaranteed to be present (checked above)
                        let text = self.content_text.as_deref().unwrap_or("");
                        let header = utils::format_metadata_header(m, *header_type, sep.as_deref(), text)?;
                        let full_content = header + text;
                        let mut fs = self.root.fs.lock().unwrap();
                        if let Some(p) = content_path.parent() {
                            fs.create_dir_all(p)?;
                        }
                        fs.writer(&content_path)?.write_all(full_content.as_bytes())?;
                    }
                    EntityMeta::None => {} // filtered in with_metadata
                }
            }
        }

        // Build nested children
        for nested in self.nested_children {
            nested.build_internal(&resolved_type)?;
        }

        Ok(LiveEntity {
            root: self.root,
            path: own_path,
            node_type: return_node_type,
        })
    }
}

impl LiveEntity {
    /// Creates a new LiveEntity handle.
    pub fn new(
        fs: Arc<Mutex<dyn Xfs + Send + Sync>>,
        base_path: PathBuf,
        path: EntityPath,
        node_type: String,
        schema: Arc<Schema>,
    ) -> Self {
        Self {
            root: Arc::new(LiveEntityRoot {
                fs,
                base_path,
                schema,
            }),
            path,
            node_type,
        }
    }

    /// Loads a schema from 'schema.toml' in the given directory and returns a root LiveEntity.
    ///
    /// # Errors
    ///
    /// Returns an error if the schema or root entity cannot be loaded.
    pub fn load_from_root(fs: Arc<Mutex<dyn Xfs + Send + Sync>>, root_path: PathBuf) -> anyhow::Result<Self> {
        let schema_path = root_path.join("schema.toml");
        let schema = Arc::new(Schema::load_from_file(&*fs.lock().unwrap(), &schema_path)?);
        Ok(Self::new(
            fs,
            root_path,
            EntityPath::empty(),
            "Auto".to_string(),
            schema,
        ))
    }

    /// Returns the logical path of this entity.
    pub fn path(&self) -> &EntityPath {
        &self.path
    }

    /// Returns the type name of this entity.
    pub fn node_type(&self) -> &str {
        &self.node_type
    }

    /// Returns the actual type name of this entity, resolving "Auto" if necessary.
    ///
    /// # Errors
    ///
    /// Returns an error if resolution fails or if metadata is missing/invalid for Auto type.
    pub fn actual_type(&self) -> anyhow::Result<String> {
        if self.node_type != "Auto" {
            return Ok(self.node_type.clone());
        }

        let meta = self.metadata()?;
        let m = meta.metadata().ok_or_else(|| {
            anyhow::anyhow!(
                "Entity at {:?} has Auto type but no metadata",
                self.on_disk_path()
            )
        })?;

        let t = m.get_str("type")?.ok_or_else(|| {
            anyhow::anyhow!(
                "Entity at {:?} has Auto type but metadata is missing 'type' key",
                self.on_disk_path()
            )
        })?;

        if t == "Auto" {
            bail!(
                "Entity at {:?} has metadata 'type' set to 'Auto', which is not allowed",
                self.on_disk_path()
            );
        }

        Ok(t)
    }

    /// Returns the full disk path for this entity's directory (or sibling base for Dot entries).
    fn on_disk_path(&self) -> PathBuf {
        self.path.to_pathbuf(&self.root.base_path)
    }

    /// Parallel content path: `name.md` (lives alongside the entity, not inside it).
    fn dot_content_path(&self) -> PathBuf {
        self.on_disk_path().with_added_extension("md")
    }

    /// Inside content path: `dir/content.md`.
    fn slash_content_path(&self) -> PathBuf {
        self.on_disk_path().join("content.md")
    }

    /// Parallel metadata path: `name.meta.toml`.
    fn dot_metadata_path(&self) -> PathBuf {
        self.on_disk_path().with_extension("meta.toml")
    }

    /// Inside metadata path: `dir/meta.toml`.
    fn slash_metadata_path(&self) -> PathBuf {
        self.on_disk_path().join("meta.toml")
    }

    /// Reads raw content from disk, auto-detecting Parallel vs Inside layout.
    ///
    /// Returns `(raw_text, is_parallel, path_used)`. Returns an error if both
    /// Parallel and Inside content files exist simultaneously.
    fn get_content_info(&self) -> anyhow::Result<(Option<String>, bool, PathBuf)> {
        let fs = self.root.fs.lock().unwrap();
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
                .map(|(m, s, a, h)| (Some(EntityMeta::InHeader(m, s, h)), a))
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
        let is_root = self.path.entries.is_empty();

        // Read content info first (acquires and releases lock internally).
        let (content_str, _, _) = self.get_content_info()?;

        let metadata_from_content = content_str.and_then(|c| {
            utils::parse_header(&c).map(|(m, s, _, h)| EntityMeta::InHeader(m, s, h))
        });

        // 2. Try loading from meta.toml files
        let dot_metadata_file = self.dot_metadata_path();
        let slash_metadata_file = self.slash_metadata_path();

        let fs = self.root.fs.lock().unwrap();
        let dot_metadata = if !is_root {
            utils::try_load_file_as_metadata(&*fs, &dot_metadata_file)?
        } else {
            None
        };
        let slash_metadata = utils::try_load_file_as_metadata(&*fs, &slash_metadata_file)?;
        drop(fs);

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
        let is_root = self.path.entries.is_empty();

        // Collect child paths with the lock held, then release before calling actual_type().
        let children_paths = {
            let fs = self.root.fs.lock().unwrap();
            let dot_children = if !is_root {
                utils::find_dot_children(&*fs, &self.root.base_path, &self.path)?
            } else {
                vec![]
            };
            let slash_children = utils::find_slash_children(&*fs, &self.root.base_path, &self.path)?;
            dot_children
                .into_iter()
                .chain(slash_children)
                .collect::<Vec<EntityPath>>()
        }; // lock released here

        let actual_type = self.actual_type()?;
        let entity_type_descriptor = self.root.schema.get_entity_type(&actual_type)?;

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
        if let EntityMeta::InHeader(m, sep, header_type) = current_meta {
            to_write.push_str(&utils::format_metadata_header(&m, header_type, sep.as_deref(), new_content)?);
        }
        to_write.push_str(new_content);

        let final_path = if content_str.is_none() {
            let fs = self.root.fs.lock().unwrap();
            if self.path.entries.is_empty() || fs.is_dir(&self.on_disk_path()) {
                self.slash_content_path()
            } else {
                self.dot_content_path()
            }
            // fs dropped here
        } else {
            path
        };

        let mut fs = self.root.fs.lock().unwrap();
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
        let (content_str, _, content_path) = self.get_content_info()?;

        let mut fs = self.root.fs.lock().unwrap();

        if let EntityMeta::InHeader(_, _, _) = current_meta {
            if !matches!(meta, EntityMeta::InHeader(_, _, _)) {
                if let Some(c) = &content_str {
                     let a = match utils::parse_header(&c) {
                         Some((_, _, a, _)) => a,
                         None => c.clone(),
                     };
                     let mut writer = fs.writer(&content_path)?;
                     writer.write_all(a.as_bytes())?;
                }
            }
        }

        match meta {
            EntityMeta::None => {
                if let EntityMeta::Parallel(_) = current_meta {
                    let _ = fs.remove_file(&self.dot_metadata_path());
                } else if let EntityMeta::Inside(_) = current_meta {
                    let _ = fs.remove_file(&self.slash_metadata_path());
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
            EntityMeta::InHeader(m, sep, header_type) => {
                let content_body = current_content.content().unwrap_or("");
                let to_write = utils::format_metadata_header(&m, header_type, sep.as_deref(), content_body)? + content_body;

                let final_path = if current_content.is_none() {
                     if self.path.entries.is_empty() {
                         self.slash_content_path()
                     } else {
                         self.dot_content_path()
                     }
                } else {
                    content_path
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
        let mut fs = self.root.fs.lock().unwrap();

        if !recursive {
            let children = utils::find_dot_children(&*fs, &self.root.base_path, &self.path)?;
            let slash_children = utils::find_slash_children(&*fs, &self.root.base_path, &self.path)?;
            if !children.is_empty() || !slash_children.is_empty() {
                bail!("Entity is not empty and recursive delete not requested");
            }
        }

        // 1. Delete content files
        let _ = fs.remove_file(&self.dot_content_path());
        let _ = fs.remove_file(&self.slash_content_path());

        // 2. Delete metadata files
        let _ = fs.remove_file(&self.dot_metadata_path());
        let _ = fs.remove_file(&self.slash_metadata_path());

        // 3. Delete the directory if it exists
        let on_disk = self.on_disk_path();
        if fs.is_dir(&on_disk) {
            fs.remove_dir_all(&on_disk)?;
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
                fs.remove_dir_all(&path)?;
            } else {
                fs.remove_file(&path)?;
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

        let mut fs = self.root.fs.lock().unwrap();

        let mut moved_anything = false;

        let dot_content = self.dot_content_path();
        if fs.is_file(&dot_content) {
            let new_dot_content = new_path.to_pathbuf(&self.root.base_path).with_added_extension("md");
            if let Some(parent) = new_dot_content.parent() {
                fs.create_dir_all(parent)?;
            }
            fs.rename(&dot_content, &new_dot_content)?;
            moved_anything = true;
        }

        let dot_metadata = self.dot_metadata_path();
        if fs.is_file(&dot_metadata) {
            let new_dot_metadata = new_path.to_pathbuf(&self.root.base_path).with_extension("meta.toml");
            if let Some(parent) = new_dot_metadata.parent() {
                fs.create_dir_all(parent)?;
            }
            fs.rename(&dot_metadata, &new_dot_metadata)?;
            moved_anything = true;
        }

        if fs.is_dir(&old_on_disk) {
            if let Some(parent) = new_on_disk.parent() {
                fs.create_dir_all(parent)?;
            }
            fs.rename(&old_on_disk, &new_on_disk)?;
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
            fs.rename(&old_child, &new_child)?;
            moved_anything = true;
        }

        if !moved_anything {
            bail!("Nothing found to move at {:?}", old_on_disk);
        }

        self.path = new_path;
        Ok(())
    }

    /// Creates a builder for a new child entity.
    ///
    /// The child's node type is inferred from the parent schema; call
    /// [`ChildBuilder::with_type`] if the schema cannot determine it.
    /// All validation and disk writes happen in [`ChildBuilder::build`].
    pub fn create_child(&self, entry: EntityPathEntry) -> ChildBuilder {
        ChildBuilder {
            root: self.root.clone(),
            parent_path: self.path.clone(),
            parent_node_type: self.node_type.clone(),
            entry,
            node_type_override: None,
            content_text: None,
            content_layout: ChildContentLayout::Inferred,
            metadata: None,
            nested_children: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::HeaderType;
    use inscenerator_xfs::mockfs;
    use inscenerator_xfs::XfsReadOnly;
    use crate::schema::ChildEntityRules;
    use crate::schema::EntityTypeDescription;
    use std::path::Path;
    use std::sync::{Arc, Mutex};

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

    fn setup_schema() -> Arc<Schema> {
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
        Arc::new(schema)
    }

    #[test]
    fn test_live_entity_yaml_write() {
        let fs = mockfs::MockFS::new();
        let fs = Arc::new(Mutex::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        let mut meta_val = toml::map::Map::new();
        meta_val.insert("key".to_string(), toml::Value::String("val".to_string()));
        let meta = crate::entity::Metadata { value: toml::Value::Table(meta_val) };

        live.set_metadata(EntityMeta::InHeader(meta, None, HeaderType::Yaml)).unwrap();
        live.set_content("Hello").unwrap();

        let content = crate::entity::utils::try_load_file_as_string(&*live.root.fs.lock().unwrap(), &PathBuf::from("foo/entity1.md")).unwrap().unwrap();
        assert!(content.contains("---\nkey: val\n---\n"));
    }

    #[test]
    fn test_live_entity_read() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "```toml\nkey = \"val\"\n```\n---\nHello");
        create_file_with_content(&mut fs, "foo/entity1", "child1.md", "Child content");
        let fs = Arc::new(Mutex::new(fs));
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
        if let EntityMeta::InHeader(m, sep, _) = meta {
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
        let fs = Arc::new(Mutex::new(fs));
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
        live.create_child(EntityPathEntry::Slash("child1".to_string()))
            .build()
            .unwrap();
        assert_eq!(live.children().unwrap().len(), 1);
    }

    #[test]
    fn test_conflict_detection() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Inside");
        create_file_with_content(&mut fs, "foo", "entity1.md", "Parallel");
        let fs = Arc::new(Mutex::new(fs));
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
    fn test_metadata_cleanup() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.meta.toml", "a = 1");
        let fs = Arc::new(Mutex::new(fs));
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
    fn test_live_entity_delete() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo/entity1", "content.md", "Hello");
        let fs = Arc::new(Mutex::new(fs));
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
    fn test_live_entity_move() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", "Content");
        let fs = Arc::new(Mutex::new(fs));
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

    #[test]
    fn test_live_entity_auto_type_resolution() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        let fs = Arc::new(Mutex::new(fs));

        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let live = LiveEntity::new(
            fs,
            PathBuf::from("project"),
            EntityPath::empty(),
            "Auto".to_string(),
            schema,
        );

        assert_eq!(live.actual_type().unwrap(), "Project");
    }

    #[test]
    fn test_create_child_slash_creates_directory() {
        let fs = mockfs::MockFS::new();
        let fs = Arc::new(Mutex::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(),
            schema,
        );

        live.create_child(EntityPathEntry::Slash("child".to_string()))
            .build()
            .unwrap();

        assert!(fs.lock().unwrap().is_dir(&PathBuf::from("foo/parent/child")));
    }

    #[test]
    fn test_create_child_slash_errors_if_directory_exists() {
        let mut fs = mockfs::MockFS::new();
        fs.create_dir_all(&PathBuf::from("foo/parent/child")).unwrap();
        let fs = Arc::new(Mutex::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(),
            schema,
        );

        let err = live
            .create_child(EntityPathEntry::Slash("child".to_string()))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("already exists"), "got: {}", err);
    }

    #[test]
    fn test_create_child_dot_on_root_errors() {
        let fs = mockfs::MockFS::new();
        let fs = Arc::new(Mutex::new(fs));
        let schema = setup_schema();

        let root = LiveEntity::new(
            fs.clone(),
            PathBuf::from("project"),
            EntityPath::empty(),
            "Type".to_string(),
            schema,
        );

        let err = root
            .create_child(EntityPathEntry::Dot("notes".to_string()))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("Root entities may only have Slash children"), "got: {}", err);
    }

    #[test]
    fn test_type_inferred_from_schema_rule() {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^child_".to_string(),
                node_type: "Child".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(),
            schema,
        );

        let child = live
            .create_child(EntityPathEntry::Slash("child_one".to_string()))
            .build()
            .unwrap();

        assert_eq!(child.node_type, "Child");
    }

    #[test]
    fn test_type_resolution_errors_on_unexpected_child() {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^child_".to_string(),
                node_type: "Child".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Slash("other".to_string()))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("Unexpected child"), "got: {}", err);
    }

    #[test]
    fn test_with_type_matching_concrete_rule_is_accepted() {
        // with_type("Child") matches what the schema says — no error
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^child_".to_string(),
                node_type: "Child".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let child = live
            .create_child(EntityPathEntry::Slash("child_one".to_string()))
            .with_type("Child") // matches schema — should succeed
            .build()
            .unwrap();

        assert_eq!(child.node_type, "Child");
    }

    #[test]
    fn test_type_resolution_errors_when_allow_additional_needs_with_type() {
        // allow_additional = true but no rule match and no with_type() => error
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Slash("anything".to_string()))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("with_type"), "got: {}", err);
    }

    #[test]
    fn test_with_type_override_for_auto_slot() {
        // allow_additional=true + with_type("Chapter") → write meta.toml, return node_type="Auto"
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(),
            schema,
        );

        let child = live
            .create_child(EntityPathEntry::Slash("item".to_string()))
            .with_type("Chapter")
            .build()
            .unwrap();

        // Returned handle has node_type = "Auto" (reads from metadata at load time)
        assert_eq!(child.node_type, "Auto");

        // meta.toml written with type = "Chapter"
        let meta_content = {
            let fs_guard = fs.lock().unwrap();
            crate::entity::utils::try_load_file_as_string(
                &*fs_guard,
                &PathBuf::from("foo/root/item/meta.toml"),
            )
            .unwrap()
            .unwrap()
        };
        assert!(meta_content.contains("type = \"Chapter\""), "got: {}", meta_content);

        // actual_type() resolves to "Chapter"
        assert_eq!(child.actual_type().unwrap(), "Chapter");
    }

    #[test]
    fn test_with_type_conflict_errors() {
        // Schema says "Scene", but with_type("Chapter") conflicts
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "Scene".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        schema.add_entity_type(EntityTypeDescription {
            name: "Scene".to_string(),
            children: vec![],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Slash("thing".to_string()))
            .with_type("Chapter")  // conflicts: schema says "Scene"
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("mismatch"), "got: {}", err);
    }

    #[test]
    fn test_with_type_errors_if_metadata_value_is_not_a_table() {
        // Auto slot + with_type("X") + metadata whose toml::Value is not a Table => error
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        // Deliberately construct a non-Table Metadata value
        let bad_meta = Metadata { value: toml::Value::String("not a table".to_string()) };
        let err = live
            .create_child(EntityPathEntry::Slash("item".to_string()))
            .with_type("Chapter")
            .with_metadata_inside(bad_meta)
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("table"), "got: {}", err);
    }

    #[test]
    fn test_live_entity_ignore_schema_toml() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "schema.toml", "");
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        let fs = Arc::new(Mutex::new(fs));

        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "Type".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let live = LiveEntity::new(
            fs,
            PathBuf::from("project"),
            EntityPath::empty(),
            "Project".to_string(),
            schema,
        );

        let children = live.children().unwrap();
        // Should not include schema.toml even with permissive regex and allow_additional
        assert_eq!(children.len(), 0);
    }

    #[test]
    fn test_with_content_slash_writes_inside() {
        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        live.create_child(EntityPathEntry::Slash("child".to_string()))
            .with_content("Hello Inside")
            .build()
            .unwrap();

        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent/child/content.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Hello Inside");
    }

    #[test]
    fn test_with_content_dot_writes_parallel() {
        let mut raw_fs = mockfs::MockFS::new();
        // parent dir must exist so the Dot existence check's read_dir can scan it
        raw_fs.create_dir_all(&PathBuf::from("foo/parent")).unwrap();
        let fs = Arc::new(Mutex::new(raw_fs));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        live.create_child(EntityPathEntry::Dot("notes".to_string()))
            .with_content("Hello Parallel")
            .build()
            .unwrap();

        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent.notes.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Hello Parallel");
    }

    #[test]
    fn test_with_content_inside_forces_inside_layout() {
        // Dot child but forced Inside layout — writes to own_disk_path/content.md
        let mut raw_fs = mockfs::MockFS::new();
        raw_fs.create_dir_all(&PathBuf::from("foo/parent")).unwrap();
        let fs = Arc::new(Mutex::new(raw_fs));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        live.create_child(EntityPathEntry::Dot("notes".to_string()))
            .with_content_inside("Forced Inside")
            .build()
            .unwrap();

        // own_disk_path for Dot("notes") under foo/parent = foo/parent.notes
        // Inside: foo/parent.notes/content.md
        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent.notes/content.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Forced Inside");
    }

    #[test]
    fn test_with_content_parallel_forces_parallel_layout() {
        // Slash child but forced Parallel layout — writes to own_disk_path.with_added_extension("md")
        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        live.create_child(EntityPathEntry::Slash("child".to_string()))
            .with_content_parallel("Forced Parallel")
            .build()
            .unwrap();

        // Parallel path for Slash entry: own_disk_path = foo/parent/child → foo/parent/child.md
        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent/child.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Forced Parallel");
    }

    #[test]
    fn test_with_metadata_inside_slash_child() {
        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        let meta = Metadata { value: toml::from_str("title = \"Test\"").unwrap() };
        live.create_child(EntityPathEntry::Slash("child".to_string()))
            .with_metadata_inside(meta)
            .build()
            .unwrap();

        let raw = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent/child/meta.toml"),
        ).unwrap().unwrap();
        assert!(raw.contains("title"), "got: {}", raw);
    }

    #[test]
    fn test_with_metadata_parallel_dot_child() {
        let mut raw_fs = mockfs::MockFS::new();
        raw_fs.create_dir_all(&PathBuf::from("foo/parent")).unwrap();
        let fs = Arc::new(Mutex::new(raw_fs));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        let meta = Metadata { value: toml::from_str("note = \"yes\"").unwrap() };
        live.create_child(EntityPathEntry::Dot("notes".to_string()))
            .with_metadata_parallel(meta)
            .build()
            .unwrap();

        let raw = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent.notes.meta.toml"),
        ).unwrap().unwrap();
        assert!(raw.contains("note"), "got: {}", raw);
    }

    #[test]
    fn test_dot_child_with_no_content_metadata_or_children_errors() {
        let mut raw_fs = mockfs::MockFS::new();
        raw_fs.create_dir_all(&PathBuf::from("foo/parent")).unwrap();
        let fs = Arc::new(Mutex::new(raw_fs));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Dot("notes".to_string()))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("nothing to write"), "got: {}", err);
    }

    #[test]
    fn test_dot_child_errors_if_files_exist() {
        let mut raw_fs = mockfs::MockFS::new();
        raw_fs.create_dir_all(&PathBuf::from("foo/parent")).unwrap();
        raw_fs.add_r(
            &PathBuf::from("foo/parent.notes.md"),
            b"existing".to_vec(),
        ).unwrap();
        let fs = Arc::new(Mutex::new(raw_fs));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Dot("notes".to_string()))
            .with_content("text")
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("already exists"), "got: {}", err);
    }

    #[test]
    fn test_with_type_merges_type_into_existing_metadata() {
        // allow_additional slot + with_type("Chapter") + with_metadata_inside(m)
        // => meta.toml contains both "title" and "type" keys
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![],
            allow_additional: true,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let meta = Metadata {
            value: toml::from_str("title = \"My Chapter\"").unwrap(),
        };
        live.create_child(EntityPathEntry::Slash("item".to_string()))
            .with_type("Chapter")
            .with_metadata_inside(meta)
            .build()
            .unwrap();

        let raw = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/root/item/meta.toml"),
        ).unwrap().unwrap();
        assert!(raw.contains("type"), "missing type key: {}", raw);
        assert!(raw.contains("title"), "missing title key: {}", raw);
    }

    #[test]
    fn test_inheader_metadata_without_content_errors() {
        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema,
        );

        let meta = Metadata { value: toml::from_str("key = \"val\"").unwrap() };
        let err = live
            .create_child(EntityPathEntry::Slash("child".to_string()))
            .with_metadata(EntityMeta::InHeader(meta, None, crate::entity::HeaderType::Yaml))
            .build()
            .unwrap_err();
        assert!(err.to_string().contains("InHeader"), "got: {}", err);
    }

    #[test]
    fn test_with_child_creates_nested_structure() {
        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let schema = setup_schema();
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Type".to_string(), schema,
        );

        live.create_child(EntityPathEntry::Slash("chapter".to_string()))
            .with_child(EntityPathEntry::Slash("scene".to_string()), |b| {
                b.with_content("Scene content")
            })
            .build()
            .unwrap();

        // chapter/ directory created
        assert!(fs.lock().unwrap().is_dir(&PathBuf::from("foo/root/chapter")));
        // scene/ directory and content.md created
        assert!(fs.lock().unwrap().is_dir(&PathBuf::from("foo/root/chapter/scene")));
        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/root/chapter/scene/content.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Scene content");
    }

    #[test]
    fn test_with_child_type_resolution_uses_outer_resolved_type() {
        // Outer: "Parent" has rule "^ch_" → "Chapter"
        // "Chapter" has rule "^sc_" → "Scene"
        // Verify nested child is resolved as "Scene" based on outer child's type "Chapter"
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^ch_".to_string(),
                node_type: "Chapter".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^sc_".to_string(),
                node_type: "Scene".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        schema.add_entity_type(EntityTypeDescription {
            name: "Scene".to_string(),
            children: vec![],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let chapter = live
            .create_child(EntityPathEntry::Slash("ch_one".to_string()))
            .with_child(EntityPathEntry::Slash("sc_one".to_string()), |b| b)
            .build()
            .unwrap();

        assert_eq!(chapter.node_type, "Chapter");

        let scenes = chapter.children().unwrap();
        assert_eq!(scenes.len(), 1);
        assert_eq!(scenes[0].node_type, "Scene");
    }

    #[test]
    fn test_auto_schema_type_without_with_type_errors() {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: ".*".to_string(),
                node_type: "Auto".to_string(),
                required: false,
                multiple: true,
            }],
            allow_additional: false,
        });
        let schema = Arc::new(schema);

        let fs = Arc::new(Mutex::new(mockfs::MockFS::new()));
        let live = LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let err = live
            .create_child(EntityPathEntry::Slash("thing".to_string()))
            .build() // no with_type() call
            .unwrap_err();
        assert!(err.to_string().contains("Auto"), "got: {}", err);
    }
}
