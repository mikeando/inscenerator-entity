use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::fmt;

use anyhow::bail;
use inscenerator_xfs::Xfs;

use std::io::Write;

use crate::entity::{
    utils, EntityContent, EntityMeta, EntityPath, EntityPathEntry, MetaOrigin, Metadata,
};
use crate::discovery;
use crate::findings::{Finding, FindingKindId, FindingPolicy, FindingSink};
use crate::placement::{self, ContentLocation, Layout, MetaLocation};
use crate::reading;
use crate::schema::Schema;

/// Shared context for a tree of LiveEntities.
pub struct LiveEntityRoot {
    /// The underlying filesystem.
    pub fs: Arc<Mutex<dyn Xfs + Send + Sync>>,
    /// The base path on disk for this entity tree.
    pub base_path: PathBuf,
    /// The schema defining entity types and rules.
    pub schema: Arc<Schema>,
    /// How severely each kind of drift is treated. Tolerant by default (D6).
    pub policy: FindingPolicy,
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
    /// The layout of the parent *instance* this handle was reached through, used when
    /// this node's own type declares none (§2.1). `Inside` for the root, and for a
    /// handle built directly by address, which has no parent to inherit from.
    ///
    /// Static — derived from schema and ancestry, never from disk — so unlike a snapshot
    /// of what is on disk it cannot go stale.
    pub inherited_layout: Layout,
}

/// Findings about the node's own identity. Every accessor needs the type resolved, so
/// every accessor answers for a type that disagrees with the schema.
const TYPE_FINDINGS: &[FindingKindId] = &[FindingKindId::TypeMismatch];

/// Findings [`LiveEntity::content`] answers for: which of the two content files won, and
/// whether it is where the layout intends.
const CONTENT_FINDINGS: &[FindingKindId] = &[
    FindingKindId::TypeMismatch,
    FindingKindId::StrayContent,
    FindingKindId::ContentLocationNonconformance,
];

/// Findings [`LiveEntity::metadata`] answers for: everything about the sources it returns.
const METADATA_FINDINGS: &[FindingKindId] = &[
    FindingKindId::TypeMismatch,
    FindingKindId::MalformedMetadata,
    FindingKindId::MetadataKeyConflict,
    FindingKindId::SplitMetadata,
    FindingKindId::MetadataLocationNonconformance,
];

/// What a node has actually established on disk. §4.1.
///
/// Used for deciding where a *write* should land; nothing in the read semantics of an
/// entity depends on it.
#[derive(Debug, Clone, PartialEq)]
pub struct ObservedPlacement {
    /// `None` when no content file exists, in which case intent decides.
    pub content: Option<ContentLocation>,
    /// The metadata sources present, in `MetaLocation` order. Empty means none.
    pub metadata: Vec<MetaLocation>,
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
    /// The layout of the parent instance, passed to the child it builds (§2.1).
    parent_inherited_layout: Layout,
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
    /// `EntityMeta::default()` clears any previously set metadata.
    /// Last call wins.
    pub fn with_metadata(mut self, meta: EntityMeta) -> Self {
        if meta.is_none() {
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
        self.metadata = Some(EntityMeta::unplaced(MetaOrigin::InsideSidecar, meta));
        self
    }

    /// Sets metadata to be written alongside the entity (`name.meta.toml`).
    ///
    /// Last call wins.
    pub fn with_metadata_parallel(mut self, meta: Metadata) -> Self {
        self.metadata = Some(EntityMeta::unplaced(MetaOrigin::ParallelSidecar, meta));
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
            parent_inherited_layout: self.parent_inherited_layout,
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
            inherited_layout: self.parent_inherited_layout,
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
                let meta_value = meta.single().and_then(|s| s.metadata()).map(|m| &m.value);
                if let Some(v) = meta_value {
                    if !v.is_table() {
                        bail!("Metadata value must be a TOML table to merge 'type' key");
                    }
                }
            }
        }

        // Merge type key into existing metadata for Auto-override
        if is_auto_override && self.metadata.is_some() {
            // (Table check already done above — if not a Table, we already bailed.)
            if let Some(m) = self.metadata.as_mut().and_then(EntityMeta::single_parsed_mut) {
                if let toml::Value::Table(ref mut table) = m.value {
                    table.insert("type".to_string(), toml::Value::String(resolved_type.clone()));
                }
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
        if self.metadata.as_ref().and_then(|m| m.source_at(MetaLocation::InHeader)).is_some() {
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
        let content_layout_used = match self.content_layout {
            ChildContentLayout::Inside => ContentLocation::Inside,
            ChildContentLayout::Parallel => ContentLocation::Parallel,
            ChildContentLayout::Inferred => match &self.entry {
                EntityPathEntry::Slash(_) => ContentLocation::Inside,
                EntityPathEntry::Dot(_) => ContentLocation::Parallel,
            },
        };
        let content_path = placement::content_path(&self.root.base_path, &own_path, content_layout_used);

        // --- Content write ---
        // Skip plain content write when InHeader is used (written together with header below)
        let is_inheader = self
            .metadata
            .as_ref()
            .and_then(|m| m.source_at(MetaLocation::InHeader))
            .is_some();
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
            for source in self.metadata.iter().flat_map(EntityMeta::sources) {
                let Some(m) = source.metadata() else { continue };
                let (path, text) = match &source.origin {
                    MetaOrigin::InsideSidecar => {
                        (own_disk_path.join("meta.toml"), toml::to_string(&m.value)?)
                    }
                    MetaOrigin::ParallelSidecar => (
                        own_disk_path.with_added_extension("meta.toml"),
                        toml::to_string(&m.value)?,
                    ),
                    MetaOrigin::Header { header_type, separator } => {
                        // Content is guaranteed to be present (checked above)
                        let body = self.content_text.as_deref().unwrap_or("");
                        let header = utils::format_metadata_header(
                            m,
                            *header_type,
                            separator.as_deref(),
                            body,
                        )?;
                        (content_path.clone(), header + body)
                    }
                };
                let mut fs = self.root.fs.lock().unwrap();
                if let Some(p) = path.parent() {
                    fs.create_dir_all(p)?;
                }
                fs.writer(&path)?.write_all(text.as_bytes())?;
            }
        }

        // Build nested children
        for nested in self.nested_children {
            nested.build_internal(&resolved_type)?;
        }

        Ok(LiveEntity {
            root: self.root,
            path: own_path,
            // TODO(Task 10): the child's layout is resolved from its type and the parent
            // instance, not guessed from its edge. Until then this mirrors the layout the
            // write above used, so a handle reads back what the builder just wrote.
            node_type: return_node_type,
            inherited_layout: match content_layout_used {
                ContentLocation::Inside => Layout::Inside,
                ContentLocation::Parallel => Layout::Parallel,
            },
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
                policy: FindingPolicy::default(),
            }),
            path,
            node_type,
            inherited_layout: Layout::Inside,
        }
    }

    /// Sets the drift policy for this handle and every handle reached through it.
    ///
    /// Call it before descending: handles already obtained keep the policy they were
    /// built with.
    #[must_use]
    pub fn with_policy(self, policy: FindingPolicy) -> Self {
        let root = Arc::new(LiveEntityRoot {
            fs: self.root.fs.clone(),
            base_path: self.root.base_path.clone(),
            schema: self.root.schema.clone(),
            policy,
        });
        Self { root, ..self }
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

    /// Returns the actual type name of this entity, resolving "Auto" from its metadata.
    ///
    /// # Errors
    ///
    /// Returns an error if the node cannot be read, or if it is `Auto` and its metadata
    /// records no type.
    pub fn actual_type(&self) -> anyhow::Result<String> {
        if self.node_type != "Auto" {
            return Ok(self.node_type.clone());
        }
        Ok(self.read(TYPE_FINDINGS)?.actual_type)
    }

    /// A sink carrying this tree's policy.
    fn sink(&self) -> FindingSink {
        FindingSink::new(self.root.policy.clone())
    }

    /// Reads this node through the reader the eager loader also uses, so a handle and a
    /// loaded tree can never disagree about what is here (C2).
    fn read_into(&self, sink: &mut FindingSink) -> anyhow::Result<reading::NodeRead> {
        let fs = self.root.fs.lock().unwrap();
        reading::read_node(
            &*fs,
            &self.root.schema,
            &self.root.base_path,
            &self.path,
            &self.node_type,
            self.inherited_layout,
            sink,
        )
    }

    /// Reads this node, raising only the findings the calling accessor answers for.
    ///
    /// A handle reads the whole node whatever you ask it — typing needs the metadata,
    /// which may itself live in the content file — but a lazy reader must abort only the
    /// accessor that produced the finding (D6). Asking for the children of a node whose
    /// sidecar is unparseable is a fair question, and gets an answer.
    fn read(&self, owned: &[FindingKindId]) -> anyhow::Result<reading::NodeRead> {
        let mut collected = FindingSink::new(FindingPolicy::tolerant());
        let node = self.read_into(&mut collected)?;

        let mut sink = self.sink();
        for finding in collected.into_findings() {
            if owned.contains(&finding.kind.id()) {
                sink.report(finding)?;
            }
        }
        Ok(node)
    }

    /// The layout this node's type intends, falling back to the layout of the parent
    /// instance it was reached through (§2.1). Always `Inside` for the root.
    ///
    /// # Errors
    ///
    /// Returns an error if the node cannot be read or its type is not in the schema.
    pub fn intended_layout(&self) -> anyhow::Result<Layout> {
        Ok(self.read(TYPE_FINDINGS)?.layout)
    }

    /// What this node has established on disk, for deciding where a write should land.
    ///
    /// # Errors
    ///
    /// Returns an error if the node cannot be read.
    pub fn observed(&self) -> anyhow::Result<ObservedPlacement> {
        let node = self.read(TYPE_FINDINGS)?;
        let content = match node.content {
            EntityContent::Parallel(_) => Some(ContentLocation::Parallel),
            EntityContent::Inside(_) => Some(ContentLocation::Inside),
            EntityContent::None => None,
        };
        Ok(ObservedPlacement { content, metadata: node.metadata.locations() })
    }

    /// Everything §9.1 has to say about *this* node, probed from disk at call time.
    ///
    /// Never fails because of a finding, whatever the policy says — reporting is this
    /// method's whole job. It errors only when the node cannot be read at all.
    pub fn issues(&self) -> anyhow::Result<Vec<Finding>> {
        let mut sink = FindingSink::new(FindingPolicy::tolerant());
        let node = self.read_into(&mut sink)?;
        self.child_handles(&node, &mut sink)?;
        Ok(sink.into_findings())
    }

    /// Returns the full disk path for this entity's directory (or sibling base for Dot entries).
    fn on_disk_path(&self) -> PathBuf {
        self.path.to_pathbuf(&self.root.base_path)
    }

    /// Parallel content path: `name.md` (lives alongside the entity, not inside it).
    fn dot_content_path(&self) -> PathBuf {
        placement::content_path(&self.root.base_path, &self.path, ContentLocation::Parallel)
    }

    /// Inside content path: `dir/content.md`.
    fn slash_content_path(&self) -> PathBuf {
        placement::content_path(&self.root.base_path, &self.path, ContentLocation::Inside)
    }

    /// Parallel metadata path: `name.meta.toml` — the suffix is *appended* to the stem.
    /// Substituting it resolved a dot child onto its parent's sidecar (C1).
    fn dot_metadata_path(&self) -> PathBuf {
        placement::sidecar_path(&self.root.base_path, &self.path, MetaLocation::ParallelSidecar)
            .expect("a sidecar location always has a path")
    }

    /// Inside metadata path: `dir/meta.toml`.
    fn slash_metadata_path(&self) -> PathBuf {
        placement::sidecar_path(&self.root.base_path, &self.path, MetaLocation::InsideSidecar)
            .expect("a sidecar location always has a path")
    }

    /// The content file this node reads from: `(raw_text, is_parallel, path)`.
    ///
    /// Where both content files exist the intended layout picks one and the other is
    /// reported (§4.4); where neither exists the path is where intent says a write would
    /// go, so a caller creating content has somewhere to put it.
    fn get_content_info(&self) -> anyhow::Result<(Option<String>, bool, PathBuf)> {
        let node = self.read(CONTENT_FINDINGS)?;
        let location = match node.content {
            EntityContent::Parallel(_) => ContentLocation::Parallel,
            EntityContent::Inside(_) => ContentLocation::Inside,
            EntityContent::None => node.layout.content_location(),
        };
        let path = placement::content_path(&self.root.base_path, &self.path, location);
        let fs = self.root.fs.lock().unwrap();
        let raw = utils::try_load_file_as_string(&*fs, &path)?;
        Ok((raw, location == ContentLocation::Parallel, path))
    }

    /// Reads the content of the entity from disk.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails, or on a finding the policy rates `Error`.
    pub fn content(&self) -> anyhow::Result<EntityContent> {
        Ok(self.read(CONTENT_FINDINGS)?.content)
    }

    /// Reads the metadata of the entity from disk, as the set of sources it has.
    ///
    /// Several sources merge per key (D2). A source that did not parse is kept rather
    /// than dropped, so it can be inspected and repaired (D3).
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails, or on a finding the policy rates `Error`.
    pub fn metadata(&self) -> anyhow::Result<EntityMeta> {
        Ok(self.read(METADATA_FINDINGS)?.metadata)
    }

    /// Returns handles to the children of this entity.
    ///
    /// A child is identified by *(edge, name)*, so two children may share a name across
    /// the edges and both are returned. §4.4.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails, or on a finding the policy rates `Error` —
    /// an unexpected child, under `allow_additional = false`, among them.
    pub fn children(&self) -> anyhow::Result<Vec<LiveEntity>> {
        let node = self.read(TYPE_FINDINGS)?;
        self.child_handles(&node, &mut self.sink())
    }

    /// Child resolution, through the one resolver the eager loader also uses.
    fn child_handles(
        &self,
        node: &reading::NodeRead,
        sink: &mut FindingSink,
    ) -> anyhow::Result<Vec<LiveEntity>> {
        let ctype = self.root.schema.compiled(&node.actual_type)?;

        let resolved = {
            let fs = self.root.fs.lock().unwrap();
            discovery::resolve_children(&*fs, &self.root.base_path, &self.path, ctype, sink)?
        };

        Ok(resolved
            .into_iter()
            .map(|child| LiveEntity {
                root: self.root.clone(),
                path: child.path,
                // §7.2: a child that matched no rule has no declared type, so its own
                // metadata has to say what it is.
                node_type: child.node_type.unwrap_or_else(|| "Auto".to_string()),
                inherited_layout: node.layout,
            })
            .collect())
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
        if let Some(source) = current_meta.source_at(MetaLocation::InHeader) {
            if let (MetaOrigin::Header { header_type, separator }, Some(m)) =
                (&source.origin, source.metadata())
            {
                to_write.push_str(&utils::format_metadata_header(
                    m,
                    *header_type,
                    separator.as_deref(),
                    new_content,
                )?);
            }
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

        if current_meta.source_at(MetaLocation::InHeader).is_some() {
            if meta.source_at(MetaLocation::InHeader).is_none() {
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

        // Any sidecar the node had but the new metadata does not is removed.
        for location in current_meta.locations() {
            if meta.source_at(location).is_some() {
                continue;
            }
            match location {
                MetaLocation::InHeader => {}
                MetaLocation::ParallelSidecar => {
                    let _ = fs.remove_file(&self.dot_metadata_path());
                }
                MetaLocation::InsideSidecar => {
                    let _ = fs.remove_file(&self.slash_metadata_path());
                }
            }
        }

        for source in meta.sources() {
            let Some(m) = source.metadata() else {
                bail!(
                    "Refusing to write a metadata source that did not parse at {:?}",
                    self.on_disk_path()
                );
            };
            match &source.origin {
                MetaOrigin::ParallelSidecar => {
                    let toml_str = toml::to_string(&m.value)?;
                    let path = self.dot_metadata_path();
                    if let Some(parent) = path.parent() {
                        fs.create_dir_all(parent)?;
                    }
                    fs.writer(&path)?.write_all(toml_str.as_bytes())?;
                }
                MetaOrigin::InsideSidecar => {
                    let toml_str = toml::to_string(&m.value)?;
                    let path = self.slash_metadata_path();
                    fs.create_dir_all(path.parent().unwrap())?;
                    fs.writer(&path)?.write_all(toml_str.as_bytes())?;
                }
                MetaOrigin::Header { header_type, separator } => {
                    let content_body = current_content.content().unwrap_or("");
                    let to_write = utils::format_metadata_header(
                        m,
                        *header_type,
                        separator.as_deref(),
                        content_body,
                    )? + content_body;

                    let final_path = if current_content.is_none() {
                        if self.path.entries.is_empty() {
                            self.slash_content_path()
                        } else {
                            self.dot_content_path()
                        }
                    } else {
                        content_path.clone()
                    };

                    if let Some(parent) = final_path.parent() {
                        fs.create_dir_all(parent)?;
                    }
                    fs.writer(&final_path)?.write_all(to_write.as_bytes())?;
                }
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
            parent_inherited_layout: self.inherited_layout,
            node_type_override: None,
            content_text: None,
            content_layout: ChildContentLayout::Inferred,
            metadata: None,
            nested_children: vec![],
        }
    }
    
    /// Returns the child with this name.
    ///
    /// Resolution runs through [`Self::children`], which is what guarantees the two can
    /// never disagree about what exists (C2).
    ///
    /// # Errors
    ///
    /// Returns an error if there is no such child, or if two children share the name
    /// across the edges — they are different entities, and the name alone does not say
    /// which one is meant.
    pub fn child(&self, name: &str) -> anyhow::Result<LiveEntity> {
        let mut matched: Vec<LiveEntity> = self
            .children()?
            .into_iter()
            .filter(|c| c.path.last_name() == Some(name))
            .collect();

        match matched.len() {
            0 => bail!("No child found with name '{}'", name),
            1 => Ok(matched.remove(0)),
            _ => {
                let names: Vec<String> = matched
                    .iter()
                    .map(|c| format!("'{}'", c.path.local_path().display()))
                    .collect();
                bail!(
                    "Two entities here are named '{}': {}. They are different entities \
                     that happen to share a name, so rename one of them or ask for the \
                     one you want by its own path.",
                    name,
                    names.join(" and ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::HeaderType;
    use inscenerator_xfs::mockfs;
    use inscenerator_xfs::XfsReadOnly;
    use crate::placement::Edge;
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: true,
            layout: None,
            ignore: vec![],
        }).unwrap();
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

        live.set_metadata(EntityMeta::in_header(live.path.clone(), meta, None, HeaderType::Yaml))
            .unwrap();
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
        let source = meta
            .source_at(MetaLocation::InHeader)
            .expect("Expected InHeader metadata");
        let MetaOrigin::Header { separator, .. } = &source.origin else {
            unreachable!()
        };
        assert_eq!(
            source.metadata().unwrap().value.get("key").unwrap().as_str().unwrap(),
            "val"
        );
        assert_eq!(separator.as_deref(), Some("---\n"));

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
        live.set_metadata(EntityMeta::inside(live.path.clone(), meta.clone())).unwrap();
        assert_eq!(live.metadata().unwrap(), EntityMeta::inside(live.path.clone(), meta));

        // 3. create_child
        live.create_child(EntityPathEntry::Slash("child1".to_string()))
            .build()
            .unwrap();
        assert_eq!(live.children().unwrap().len(), 1);
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

        live.set_metadata(EntityMeta::default()).unwrap();
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
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
            layout: None,
            ignore: vec![],
        }).unwrap();
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
            layout: None,
            ignore: vec![],
        }).unwrap();
        // The written type still has to be one the schema declares — reading the child
        // back resolves its layout, and an undeclared type has none.
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![],
            allow_additional: true,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Scene".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: true,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
            layout: None,
            ignore: vec![],
        }).unwrap();
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
            .with_metadata(EntityMeta::unplaced(
                MetaOrigin::Header { header_type: crate::entity::HeaderType::Yaml, separator: None },
                meta,
            ))
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^sc_".to_string(),
                node_type: "Scene".to_string(),
                required: false,
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Scene".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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
                edge: Edge::Slash,
                multiple: true,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
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

    #[test]
    fn test_children_ignores_slash_child_in_ignore_list() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/010_chapter", "content.md", "chapter");
        create_file_with_content(&mut fs, "project/booker-data", "state.json", "{}");
        let fs = Arc::new(Mutex::new(fs));

        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
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
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        let schema = Arc::new(schema);

        let live = LiveEntity::new(fs, PathBuf::from("project"), EntityPath::empty(), "Project".to_string(), schema);
        let children = live.children().unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].node_type, "Chapter");
    }

    #[test]
    fn test_children_ignores_dot_child_in_ignore_list() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project/parent", "content.md", "parent");
        create_file_with_content(&mut fs, "project", "parent.real-child.md", "child");
        create_file_with_content(&mut fs, "project", "parent.booker-data.md", "tool data");
        let fs = Arc::new(Mutex::new(fs));

        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Parent".to_string(),
            children: vec![ChildEntityRules {
                name_regex: "^real-child$".to_string(),
                node_type: "Child".to_string(),
                required: false,
                edge: Edge::Slash,
                multiple: false,
            }],
            allow_additional: false,
            layout: None,
            ignore: vec!["booker-data".to_string()],
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Child".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec![],
        }).unwrap();
        let schema = Arc::new(schema);

        let live = LiveEntity::new(
            fs, PathBuf::from("project"),
            EntityPath::empty().extend_slash("parent"),
            "Parent".to_string(), schema,
        );
        let children = live.children().unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].node_type, "Child");
    }

    #[test]
    fn test_children_ignore_prevents_unexpected_child_error() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "meta.toml", "type = \"Project\"");
        create_file_with_content(&mut fs, "project/booker-data", "state.json", "{}");
        let fs = Arc::new(Mutex::new(fs));

        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![],
            allow_additional: false,
            layout: None,
            ignore: vec!["booker-data".to_string()],
        }).unwrap();
        let schema = Arc::new(schema);

        let live = LiveEntity::new(fs, PathBuf::from("project"), EntityPath::empty(), "Project".to_string(), schema);
        let children = live.children().unwrap();
        assert_eq!(children.len(), 0);
    }
}

#[cfg(test)]
mod live_read_tests {
    use super::*;
    use crate::findings::{FindingKind, FindingPolicy};
    use crate::placement::{ContentLocation, Edge, Layout};
    use inscenerator_xfs::mockfs;
    use std::sync::{Arc, Mutex};

    /// Root (inside) > Chapter (parallel) > Section. `notes` is declared on the slash
    /// edge and `review` on the dot edge; Section declares no layout of its own, so it
    /// takes the layout of the Chapter instance it is reached through.
    const SCHEMA: &str = r#"
[Root]
allow_additional = false
[[Root.children]]
name_regex = "^ch"
node_type = "Chapter"

[Chapter]
allow_additional = false
layout = "parallel"
[[Chapter.children]]
name_regex = "^notes$"
node_type = "Section"
edge = "slash"
[[Chapter.children]]
name_regex = "^review$"
node_type = "Section"
edge = "dot"

[Section]
allow_additional = false
children = []
"#;

    fn root_with(files: &[(&str, &str)]) -> LiveEntity {
        let mut fs = mockfs::MockFS::new();
        fs.create_dir_all(&PathBuf::from("foo")).unwrap();
        for (path, content) in files {
            let p = PathBuf::from(path);
            fs.create_dir_all(p.parent().unwrap()).unwrap();
            fs.add_r(&p, content.as_bytes().to_vec()).unwrap();
        }
        LiveEntity::new(
            Arc::new(Mutex::new(fs)),
            PathBuf::from("foo"),
            EntityPath::empty(),
            "Root".to_string(),
            Arc::new(Schema::load_from_str(SCHEMA).unwrap()),
        )
    }

    /// The `ch1` handle the tests below hang off. Reached through `child()`, so it
    /// carries whatever the root handle passes down.
    fn ch1(files: &[(&str, &str)]) -> LiveEntity {
        root_with(files).child("ch1").unwrap()
    }

    /// C2: `child()` and `children()` must never contradict each other about what
    /// exists. Two children may share a name across the edges — `(edge, name)` is what
    /// identifies a child — so both are returned, and it is the name-based lookup that
    /// refuses, rather than one of the two entities being quietly discarded.
    #[test]
    fn child_and_children_agree_on_a_duplicated_name() {
        let ch1 = ch1(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1/notes.md", "slash"),
            ("foo/ch1.notes.md", "dot"),
        ]);

        let paths: Vec<EntityPath> = ch1.children().unwrap().iter().map(|c| c.path.clone()).collect();
        assert_eq!(
            paths,
            vec![ch1.path.extend_dot("notes"), ch1.path.extend_slash("notes")]
        );

        let err = ch1.child("notes").unwrap_err().to_string();
        assert!(err.contains("ch1.notes"), "the refusal names both files: {}", err);
        assert!(err.contains("ch1/notes"), "the refusal names both files: {}", err);
    }

    /// C1 (the third site): a dot child's sidecar is its own stem with `.meta.toml`
    /// appended. Substituting resolved `ch1.review` onto `ch1.meta.toml`, so a handle
    /// on the child read — and would later overwrite — its parent's metadata.
    #[test]
    fn a_dot_childs_sidecar_is_its_own_not_its_parents() {
        let ch1 = ch1(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1.meta.toml", "owner = \"parent\""),
            ("foo/ch1.review.md", "review"),
            ("foo/ch1.review.meta.toml", "owner = \"child\""),
        ]);
        let review = ch1.child("review").unwrap();

        let owner = |e: &LiveEntity| e.metadata().unwrap().get_str("owner").unwrap();
        assert_eq!(owner(&ch1).as_deref(), Some("parent"));
        assert_eq!(owner(&review).as_deref(), Some("child"));
    }

    /// §4.4: both content files existing is drift, not a refusal. The intended layout
    /// picks one and the read succeeds; it used to fail outright.
    #[test]
    fn both_content_files_read_by_intended_layout_instead_of_failing() {
        let ch1 = ch1(&[
            ("foo/ch1.md", "parallel body"),
            ("foo/ch1/content.md", "inside body"),
        ]);

        assert_eq!(ch1.content().unwrap(), EntityContent::Parallel("parallel body".into()));
    }

    /// D2 / §4.5: two sources merge, and each key still knows which source holds it,
    /// which is what lets a later write land on the file the key already lives in.
    #[test]
    fn metadata_merges_a_header_and_a_sidecar() {
        let ch1 = ch1(&[
            ("foo/ch1.md", "```toml\nowner = \"header\"\n```\nbody"),
            ("foo/ch1.meta.toml", "n = 1"),
        ]);
        let meta = ch1.metadata().unwrap();

        assert_eq!(meta.get_str("owner").unwrap().as_deref(), Some("header"));
        assert_eq!(meta.location_of("owner"), Some(MetaLocation::InHeader));
        assert_eq!(meta.location_of("n"), Some(MetaLocation::ParallelSidecar));
    }

    /// §2.1: layout is inherited by *instance*, so a Section reached through a Chapter
    /// is Parallel even though the Section type declares no layout at all.
    #[test]
    fn intended_layout_is_inherited_from_the_parent_handle() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter"), ("foo/ch1/notes.md", "section")]);

        assert_eq!(ch1.intended_layout().unwrap(), Layout::Parallel, "declared");
        assert_eq!(
            ch1.child("notes").unwrap().intended_layout().unwrap(),
            Layout::Parallel,
            "inherited from the chapter it was reached through"
        );
    }

    /// §4.1: what a node has *established* is a different question from what its type
    /// intends. A mixed node answers differently to each, and the write side needs both.
    #[test]
    fn observed_reports_what_is_on_disk_not_what_is_intended() {
        let ch1 = ch1(&[("foo/ch1.md", "body"), ("foo/ch1/meta.toml", "k = 1")]);

        assert_eq!(ch1.intended_layout().unwrap(), Layout::Parallel);
        assert_eq!(
            ch1.observed().unwrap(),
            ObservedPlacement {
                content: Some(ContentLocation::Parallel),
                metadata: vec![MetaLocation::InsideSidecar],
            }
        );
    }

    /// D4 (lazy half): drift is reported when it is asked for, and never by failing an
    /// unrelated read. Here `review` is declared on the dot edge but sits on the slash
    /// edge — it is still a child, and the node around it is still readable.
    #[test]
    fn issues_reports_drift_without_failing_the_reads() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter"), ("foo/ch1/review.md", "on the wrong edge")]);

        assert!(ch1.issues().unwrap().iter().any(|f| matches!(
            f.kind,
            FindingKind::EdgeNonconformance { actual: Edge::Slash, intended: Edge::Dot, .. }
        )));
        assert!(ch1.content().is_ok());
        assert_eq!(ch1.children().unwrap().len(), 1);
    }

    /// D6 (lazy half): a handle reads on demand, so an `Error` finding aborts only the
    /// accessor that met it. The eager loader fails the whole tree instead.
    #[test]
    fn an_error_finding_fails_only_the_accessor_that_produced_it() {
        let ch1 = root_with(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1.meta.toml", "this = = not toml"),
            ("foo/ch1/notes.md", "section"),
        ])
        .with_policy(FindingPolicy::strict())
        .child("ch1")
        .unwrap();

        assert!(ch1.metadata().is_err());
        assert_eq!(ch1.children().unwrap().len(), 1, "the rest of the node still reads");
        assert_eq!(ch1.content().unwrap(), "chapter", "the content is still readable");
    }

    /// D3: a malformed source is never silently dropped. Reading a value *through* it
    /// refuses — the file may well hold that key — and the refusal names the file to
    /// repair, while the raw text stays reachable so a caller can repair it.
    #[test]
    fn a_malformed_source_is_reachable_but_not_readable_through() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter"), ("foo/ch1.meta.toml", "this = = not toml")]);
        let meta = ch1.metadata().unwrap();

        let err = meta.get_str("owner").unwrap_err().to_string();
        assert!(err.contains("ch1.meta.toml"), "the message names the file: {}", err);

        let bad = meta.malformed();
        assert_eq!(bad.len(), 1);
        assert!(bad[0].raw().unwrap().contains("not toml"));
    }
}
