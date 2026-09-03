use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::fmt;

use anyhow::{anyhow, bail};
use inscenerator_xfs::Xfs;

use std::io::Write;

use crate::entity::{
    utils, EntityContent, EntityMeta, EntityPath, HeaderType, MetaOrigin, MetaSource, MetaState,
    Metadata,
};
use crate::discovery;
use crate::findings::{Finding, FindingKindId, FindingPolicy, FindingSink};
use crate::placement::{self, ContentLocation, Edge, Layout, MetaLocation};
use crate::reading;
use crate::schema::{ChildMatch, Schema};

/// §4.3 for content: what the node has established wins, and only a node with no content
/// at all falls back to what its type intends.
fn resolve_content_location(node: &reading::NodeRead) -> ContentLocation {
    match node.content {
        EntityContent::Parallel(_) => ContentLocation::Parallel,
        EntityContent::Inside(_) => ContentLocation::Inside,
        EntityContent::None => node.layout.content_location(),
    }
}

/// §4.3 for one metadata key: the source that already holds it, else the node's only
/// source, else what the type intends.
///
/// The middle step is what keeps a mixed node mixed (C7) — a new key joins the sidecar
/// the node actually has rather than creating the one its layout would have chosen.
fn resolve_meta_location(node: &reading::NodeRead, key: &str) -> MetaLocation {
    if let Some(location) = node.metadata.location_of(key) {
        return location;
    }
    let locations = node.metadata.locations();
    if locations.len() == 1 {
        return locations[0];
    }
    node.layout.sidecar_location()
}

/// Front matter as it should appear above `body`.
///
/// A source that did not parse is put back inside its delimiters exactly as it was found,
/// so rewriting the body never destroys a header the library could not read (D3).
fn header_text(source: &MetaSource, body: &str) -> anyhow::Result<String> {
    let MetaOrigin::Header { header_type, separator } = &source.origin else {
        bail!("not front matter: {:?}", source.origin);
    };
    let m = match &source.state {
        MetaState::Parsed(m) => {
            return utils::format_metadata_header(m, *header_type, separator.as_deref(), body)
        }
        MetaState::Malformed { raw, .. } => raw,
    };
    let (open, close) = match header_type {
        HeaderType::Toml => ("```toml\n", "```\n"),
        HeaderType::Yaml => ("---\n", "---\n"),
    };
    let mut out = String::from(open);
    out.push_str(m);
    if !m.ends_with('\n') {
        out.push('\n');
    }
    out.push_str(close);
    match separator {
        Some(s) => out.push_str(s),
        None if !body.starts_with('\n') => out.push('\n'),
        None => {}
    }
    Ok(out)
}

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

/// Builder for creating a new child entity on disk.
///
/// Obtain via [`LiveEntity::create_child`], which takes only the child's *name*: where it
/// attaches and where its files go are resolved from the schema (§4.3), not stated by the
/// caller. [`with_edge`](Self::with_edge) and [`with_layout`](Self::with_layout) override
/// that resolution for a caller that means to (§8.1).
///
/// Call [`build`](ChildBuilder::build) to write to disk.
#[derive(Debug, Clone)]
#[must_use = "a ChildBuilder does nothing until build() is called"]
pub struct ChildBuilder {
    root: Arc<LiveEntityRoot>,
    /// Logical path of the parent entity. A nested builder does not know this until its
    /// parent's edge is resolved, so the parent fills it in at build time.
    parent_path: EntityPath,
    /// Node type of the parent (may be "Auto", resolved via actual_type() at build time).
    parent_node_type: String,
    /// The layout the parent handle inherited, used to resolve the parent's own (§2.1).
    parent_inherited_layout: Layout,
    name: String,
    node_type_override: Option<String>,
    content_text: Option<String>,
    metadata: Option<Metadata>,
    metadata_location: Option<MetaLocation>,
    edge_override: Option<Edge>,
    layout_override: Option<Layout>,
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

    /// Sets the content text. It is written wherever the resolved layout puts content.
    ///
    /// Last call wins.
    pub fn with_content(mut self, text: &str) -> Self {
        self.content_text = Some(text.to_string());
        self
    }

    /// Sets the child's metadata, written wherever the resolved layout puts a sidecar.
    ///
    /// Last call wins.
    pub fn with_metadata(mut self, meta: Metadata) -> Self {
        self.metadata = Some(meta);
        self
    }

    /// Sets the child's metadata and the location to write it to, overriding the layout.
    ///
    /// [`MetaLocation::InHeader`] requires content, since front matter has to sit above
    /// something. Last call wins.
    pub fn with_metadata_at(mut self, location: MetaLocation, meta: Metadata) -> Self {
        self.metadata = Some(meta);
        self.metadata_location = Some(location);
        self
    }

    /// Forces the edge this child attaches on, overriding what the schema resolved.
    ///
    /// The exception, not the ordinary path (§8.1): the parent's rule declares the edge,
    /// and that is what lets a caller create a child without knowing the convention.
    pub fn with_edge(mut self, edge: Edge) -> Self {
        self.edge_override = Some(edge);
        self
    }

    /// Forces this child's layout, overriding its type's and what it would inherit.
    ///
    /// See [`with_edge`](Self::with_edge): also the exception. Layout and edge are
    /// independent, which is what makes §5's shape expressible at all.
    pub fn with_layout(mut self, layout: Layout) -> Self {
        self.layout_override = Some(layout);
        self
    }

    /// Adds a nested child builder.
    ///
    /// The closure receives a fresh [`ChildBuilder`] for a child of *this* entity.
    /// Nested children are built (in order) when [`build`](Self::build) is called, by
    /// which time this entity's own path is known.
    pub fn with_child<F>(mut self, name: &str, f: F) -> Self
    where
        F: FnOnce(ChildBuilder) -> ChildBuilder,
    {
        let inner = ChildBuilder {
            root: self.root.clone(),
            // Both are filled in by build_internal, which is the first point at which
            // this entity's own path and layout are known.
            parent_path: EntityPath::empty(),
            parent_inherited_layout: Layout::Inside,
            parent_node_type: String::new(), // unused: nested builders always enter via
                                             // build_internal, never via build()
            name: name.to_string(),
            node_type_override: None,
            content_text: None,
            metadata: None,
            metadata_location: None,
            edge_override: None,
            layout_override: None,
            nested_children: vec![],
        };
        self.nested_children.push(f(inner));
        self
    }

    /// Validates configuration, writes the child entity to disk, and returns a handle to it.
    ///
    /// # Errors
    ///
    /// Returns an error if the child already exists, if the schema rejects the child name
    /// or type, if there would be nothing on disk to show for it, or if disk access fails.
    pub fn build(self) -> anyhow::Result<LiveEntity> {
        let parent_live = LiveEntity {
            root: self.root.clone(),
            path: self.parent_path.clone(),
            node_type: self.parent_node_type.clone(),
            inherited_layout: self.parent_inherited_layout,
        };
        let parent_type = parent_live.actual_type()?;
        let parent_layout = parent_live.intended_layout()?;
        self.build_internal(&parent_type, parent_layout)
    }

    fn build_internal(self, parent_type: &str, parent_layout: Layout) -> anyhow::Result<LiveEntity> {
        let parent_ctype = self.root.schema.compiled(parent_type)?;

        // --- Type (§7.1), through the one rule matcher.
        let (inferred_type, rule_index, declared_edge) = match parent_ctype.match_child(&self.name)
        {
            ChildMatch::Matched(r) => {
                (Some(r.rule.node_type.clone()), Some(r.index), r.rule.edge)
            }
            // No rule, so no declared type and no declared edge (§7.2).
            ChildMatch::Additional => (None, None, Edge::default()),
            ChildMatch::Ignored => bail!(
                "Name '{}' is ignored by type '{}', so it cannot be one of its children",
                self.name,
                parent_type
            ),
            ChildMatch::Unexpected => {
                bail!("Unexpected child '{}' in entity of type '{}'", self.name, parent_type)
            }
        };

        let resolved_type = match (&inferred_type, &self.node_type_override) {
            (Some(inferred), None) => inferred.clone(),
            (Some(inferred), Some(overridden)) if inferred == overridden => inferred.clone(),
            (Some(inferred), Some(overridden)) if inferred == "Auto" => overridden.clone(),
            (Some(inferred), Some(overridden)) => bail!(
                "Type mismatch: schema inferred '{}' but with_type specified '{}'",
                inferred,
                overridden
            ),
            (None, Some(overridden)) => overridden.clone(),
            (None, None) => bail!(
                "Child '{}' does not match any schema rule; call with_type() to specify its type",
                self.name
            ),
        };
        if resolved_type == "Auto" {
            bail!(
                "Child '{}' has schema type 'Auto' — call with_type() to specify the concrete type",
                self.name
            );
        }
        // A slot the schema did not type has to record its type in the child itself.
        let is_auto_override = inferred_type.as_deref() == Some("Auto") || inferred_type.is_none();

        // --- Edge (§4.3): an override, else the edge this name already sits on, else the
        // edge this rule's other children have settled on, else the declaration. A rule
        // already split across both edges is not evidence of anything, so it does not
        // spread (§4.5) — the declaration wins there.
        let siblings = {
            let mut sink = FindingSink::new(FindingPolicy::tolerant());
            let fs = self.root.fs.lock().unwrap();
            discovery::resolve_children(
                &*fs,
                &self.root.base_path,
                &self.parent_path,
                parent_ctype,
                &mut sink,
            )?
        };
        let edge = match (self.edge_override, siblings.iter().find(|c| c.name == self.name)) {
            (Some(edge), _) => edge,
            (None, Some(existing)) => existing.edge,
            (None, None) => {
                let established: Vec<Edge> = siblings
                    .iter()
                    .filter(|c| rule_index.is_some() && c.rule_index == rule_index)
                    .map(|c| c.edge)
                    .collect();
                let all_on = |e: Edge| !established.is_empty() && established.iter().all(|x| *x == e);
                if all_on(Edge::Dot) {
                    Edge::Dot
                } else if all_on(Edge::Slash) {
                    Edge::Slash
                } else {
                    declared_edge
                }
            }
        };
        // §3: the root has no filename to prefix, so it has no dot children.
        if self.parent_path.entries.is_empty() && edge == Edge::Dot {
            bail!("Root entities may only have Slash children");
        }

        // --- Layout (§4.3, §2.1): an override, else what the child's type declares, else
        // the layout of the parent *instance* it is being created under. Independent of
        // the edge above: coupling them is defect C3.
        let child_ctype = self.root.schema.compiled(&resolved_type)?;
        let layout = self
            .layout_override
            .or(child_ctype.desc.layout)
            .unwrap_or(parent_layout);

        let own_path = placement::child_path(&self.parent_path, &self.name, edge);
        let stem = placement::stem(&self.root.base_path, &own_path);

        // --- Existence. Any trace of this (edge, name) means the child is already there.
        if let Some(found) = self.existing_trace(&own_path, &stem)? {
            bail!("Child '{}' already exists at {:?}", self.name, found);
        }

        // --- Metadata to write, which for an untyped slot includes the type itself.
        let mut metadata = self.metadata.clone();
        if is_auto_override {
            let m = metadata.get_or_insert_with(|| Metadata {
                value: toml::Value::Table(toml::map::Map::new()),
            });
            let toml::Value::Table(table) = &mut m.value else {
                bail!("Metadata value must be a TOML table to merge 'type' key");
            };
            table.insert("type".to_string(), toml::Value::String(resolved_type.clone()));
        }
        let meta_location = self.metadata_location.unwrap_or(layout.sidecar_location());
        if metadata.is_some() && meta_location == MetaLocation::InHeader && self.content_text.is_none()
        {
            bail!("InHeader metadata requires content to be set via with_content()");
        }

        // §6: a node is what it leaves on disk. An inside node's directory is that trace
        // even when it is empty; a parallel node with nothing to write leaves none.
        let leaves_nothing =
            metadata.is_none() && self.content_text.is_none() && self.nested_children.is_empty();
        if leaves_nothing && layout == Layout::Parallel {
            bail!(
                "Child '{}' has nothing to write to disk; provide content, metadata, or children",
                self.name
            );
        }
        if layout == Layout::Inside {
            self.root.fs.lock().unwrap().create_dir_all(&stem)?;
        }

        // --- Write. Front matter shares the content file, so the two go out together.
        let mut header = String::new();
        if let Some(m) = &metadata {
            match placement::sidecar_path(&self.root.base_path, &own_path, meta_location) {
                Some(path) => {
                    write_file(&self.root, &path, &toml::to_string(&m.value)?)?;
                }
                None => {
                    let body = self.content_text.as_deref().unwrap_or("");
                    header = utils::format_metadata_header(m, HeaderType::Toml, None, body)?;
                }
            }
        }
        if let Some(text) = &self.content_text {
            let path = placement::content_path(&self.root.base_path, &own_path, layout.content_location());
            write_file(&self.root, &path, &(header + text))?;
        }

        for mut nested in self.nested_children {
            nested.parent_path = own_path.clone();
            nested.build_internal(&resolved_type, layout)?;
        }

        Ok(LiveEntity {
            root: self.root,
            path: own_path,
            node_type: if is_auto_override { "Auto".to_string() } else { resolved_type },
            // §2.1: a child created under this one inherits *this* one's layout.
            inherited_layout: layout,
        })
    }

    /// Anything on disk that this (edge, name) already occupies: its directory, either of
    /// its content files, either of its sidecars, or a dot child hanging off it.
    fn existing_trace(&self, own_path: &EntityPath, stem: &Path) -> anyhow::Result<Option<PathBuf>> {
        let base = &self.root.base_path;
        let fs = self.root.fs.lock().unwrap();
        if fs.is_dir(stem) {
            return Ok(Some(stem.to_path_buf()));
        }
        for location in [ContentLocation::Parallel, ContentLocation::Inside] {
            let path = placement::content_path(base, own_path, location);
            if fs.is_file(&path) {
                return Ok(Some(path));
            }
        }
        for location in [MetaLocation::ParallelSidecar, MetaLocation::InsideSidecar] {
            let path = placement::sidecar_path(base, own_path, location)
                .expect("a sidecar location always has a path");
            if fs.is_file(&path) {
                return Ok(Some(path));
            }
        }
        let (Some(dir), Some(name)) = (stem.parent(), stem.file_name().and_then(|n| n.to_str()))
        else {
            return Ok(None);
        };
        let prefix = format!("{}.", name);
        if let Ok(entries) = fs.read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                let is_descendant = path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.starts_with(&prefix));
                if is_descendant {
                    return Ok(Some(path));
                }
            }
        }
        Ok(None)
    }
}

/// Writes `text` to `path`, creating the directories above it.
fn write_file(root: &LiveEntityRoot, path: &Path, text: &str) -> anyhow::Result<()> {
    let mut fs = root.fs.lock().unwrap();
    if let Some(parent) = path.parent() {
        fs.create_dir_all(parent)?;
    }
    fs.writer(path)?.write_all(text.as_bytes())?;
    Ok(())
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

    /// A root handle over a schema supplied directly, rather than read from `schema.toml`.
    ///
    /// # Errors
    ///
    /// Returns an error if the root cannot be read.
    pub fn load_from_root_with(
        fs: Arc<Mutex<dyn Xfs + Send + Sync>>,
        root_path: PathBuf,
        schema: Arc<Schema>,
    ) -> anyhow::Result<Self> {
        Ok(Self::new(fs, root_path, EntityPath::empty(), "Auto".to_string(), schema))
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
    /// The content goes where this node already keeps its content; only a node with no
    /// content at all follows what its type intends (§4.3). A write is never a
    /// relocation. Front matter shares the content file, so it is written back above the
    /// new body — setting content is not an edit to metadata.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn set_content(&self, new_content: &str) -> anyhow::Result<()> {
        let node = self.read(CONTENT_FINDINGS)?;
        let path = placement::content_path(
            &self.root.base_path,
            &self.path,
            resolve_content_location(&node),
        );

        let mut to_write = match node.metadata.source_at(MetaLocation::InHeader) {
            Some(source) => header_text(source, new_content)?,
            None => String::new(),
        };
        to_write.push_str(new_content);
        self.write_file(&path, &to_write)
    }

    /// Sets one metadata key, routed per §4.5. Nothing else moves.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails, or if the key routes to a source that did
    /// not parse — the library will not merge a key into text it does not understand.
    pub fn set_meta_key(&self, key: &str, value: toml::Value) -> anyhow::Result<()> {
        self.edit_meta_key(key, Some(value))
    }

    /// Removes one metadata key from the source that holds it. A source emptied this way
    /// stays: removing a key is not removing a source, which is [`Self::clear_metadata`].
    ///
    /// # Errors
    ///
    /// As [`Self::set_meta_key`]. Removing a key the node does not have is not an error,
    /// and touches nothing.
    pub fn remove_meta_key(&self, key: &str) -> anyhow::Result<()> {
        self.edit_meta_key(key, None)
    }

    /// Reads one source, changes one key in it, and writes that source back.
    fn edit_meta_key(&self, key: &str, value: Option<toml::Value>) -> anyhow::Result<()> {
        let node = self.read(METADATA_FINDINGS)?;
        if value.is_none() && node.metadata.location_of(key).is_none() {
            return Ok(());
        }
        let location = resolve_meta_location(&node, key);

        let mut table = match node.metadata.source_at(location) {
            Some(source) => match &source.state {
                MetaState::Parsed(m) => m
                    .value
                    .as_table()
                    .cloned()
                    .ok_or_else(|| anyhow!("Metadata at {:?} is not a table", location))?,
                // D3: replacing the source wholesale is the way to repair one of these.
                MetaState::Malformed { .. } => bail!(
                    "Cannot edit metadata in {}: it is not valid TOML. Fix the file, or \
                     replace its contents outright.",
                    self.meta_file_name(location)
                ),
            },
            None => toml::map::Map::new(),
        };
        match value {
            Some(v) => {
                table.insert(key.to_string(), v);
            }
            None => {
                table.remove(key);
            }
        }
        self.write_meta_source(&node, location, &Metadata { value: toml::Value::Table(table) })
    }

    /// Replaces one metadata source outright, whatever was there before.
    ///
    /// Explicit, for a caller that means it — and the way to repair a source that did not
    /// parse (D3), since nothing here has to read what it is replacing.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn set_metadata_at(&self, location: MetaLocation, metadata: Metadata) -> anyhow::Result<()> {
        // Read with no findings owned: a wholesale replacement does not depend on the
        // metadata being readable, so a policy that rates it an error must not block the
        // one operation that fixes it.
        let node = self.read(&[])?;
        self.write_meta_source(&node, location, &metadata)
    }

    /// Removes every metadata source this node has, front matter included.
    ///
    /// The content is not metadata: it survives having a header stripped off it.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails.
    pub fn clear_metadata(&self) -> anyhow::Result<()> {
        let node = self.read(&[])?;
        for location in node.metadata.locations() {
            match placement::sidecar_path(&self.root.base_path, &self.path, location) {
                Some(path) => {
                    let mut fs = self.root.fs.lock().unwrap();
                    fs.remove_file(&path)?;
                }
                None => {
                    let body = node.content.content().unwrap_or("").to_string();
                    let path = placement::content_path(
                        &self.root.base_path,
                        &self.path,
                        resolve_content_location(&node),
                    );
                    self.write_file(&path, &body)?;
                }
            }
        }
        Ok(())
    }

    /// Writes one whole metadata source. In-header metadata shares the content file, so
    /// writing it means writing the body back out underneath it.
    fn write_meta_source(
        &self,
        node: &reading::NodeRead,
        location: MetaLocation,
        metadata: &Metadata,
    ) -> anyhow::Result<()> {
        let Some(path) = placement::sidecar_path(&self.root.base_path, &self.path, location) else {
            let body = node.content.content().unwrap_or("");
            // The shape of front matter belongs to the file it is in, not to the caller.
            let (header_type, separator) = match node.metadata.source_at(MetaLocation::InHeader) {
                Some(MetaSource { origin: MetaOrigin::Header { header_type, separator }, .. }) => {
                    (*header_type, separator.clone())
                }
                _ => (HeaderType::Toml, None),
            };
            let text = utils::format_metadata_header(
                metadata,
                header_type,
                separator.as_deref(),
                body,
            )? + body;
            let path = placement::content_path(
                &self.root.base_path,
                &self.path,
                resolve_content_location(node),
            );
            return self.write_file(&path, &text);
        };
        self.write_file(&path, &toml::to_string(&metadata.value)?)
    }

    /// The file a metadata location lives in, for a message a person has to act on.
    fn meta_file_name(&self, location: MetaLocation) -> String {
        match placement::sidecar_path(&self.root.base_path, &self.path, location) {
            Some(p) => p.display().to_string(),
            None => format!("the front matter of {}", self.on_disk_path().display()),
        }
    }

    fn write_file(&self, path: &Path, text: &str) -> anyhow::Result<()> {
        write_file(&self.root, path, text)
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

        // 1. Delete both content files and both sidecars. Which of them a node actually
        //    has is its own business (a mixed node has one of each); delete removes the
        //    node, so it removes whatever is there.
        let base = &self.root.base_path;
        for loc in [ContentLocation::Parallel, ContentLocation::Inside] {
            let _ = fs.remove_file(&placement::content_path(base, &self.path, loc));
        }
        for loc in [MetaLocation::ParallelSidecar, MetaLocation::InsideSidecar] {
            if let Some(p) = placement::sidecar_path(base, &self.path, loc) {
                let _ = fs.remove_file(&p);
            }
        }

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
    /// This **relocates and does not normalise** (§9.3): every file keeps the layout it
    /// arrived with, at the new stem. If the node's new position intends a different
    /// layout, that disagreement is a finding on the moved node — [`Self::issues`] will
    /// report it — not something this call repairs (D5). Normalisation is a separate
    /// operation.
    ///
    /// A moved handle's [`Self::inherited_layout`] is whatever it was before the move,
    /// which is stale if the node changed parents. Re-fetch through the new parent's
    /// [`Self::child`] to get one that inherits correctly.
    ///
    /// # Errors
    ///
    /// Returns an error if disk access fails or if nothing is found to move.
    pub fn move_to(&mut self, new_path: EntityPath) -> anyhow::Result<()> {
        let old_on_disk = self.on_disk_path();
        let new_on_disk = new_path.to_pathbuf(&self.root.base_path);

        let mut fs = self.root.fs.lock().unwrap();

        let mut moved_anything = false;

        // The node's own parallel files. The inside pair lives in the stem directory,
        // which the directory rename below carries; a mixed node (C7) has one of each,
        // so both passes have to run.
        let base = &self.root.base_path;
        let mut own_files = vec![(
            placement::content_path(base, &self.path, ContentLocation::Parallel),
            placement::content_path(base, &new_path, ContentLocation::Parallel),
        )];
        if let (Some(from), Some(to)) = (
            placement::sidecar_path(base, &self.path, MetaLocation::ParallelSidecar),
            placement::sidecar_path(base, &new_path, MetaLocation::ParallelSidecar),
        ) {
            own_files.push((from, to));
        }
        for (from, to) in own_files {
            if !fs.is_file(&from) {
                continue;
            }
            if let Some(parent) = to.parent() {
                fs.create_dir_all(parent)?;
            }
            fs.rename(&from, &to)?;
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
    pub fn create_child(&self, name: &str) -> ChildBuilder {
        ChildBuilder {
            root: self.root.clone(),
            parent_path: self.path.clone(),
            parent_node_type: self.node_type.clone(),
            parent_inherited_layout: self.inherited_layout,
            name: name.to_string(),
            node_type_override: None,
            content_text: None,
            metadata: None,
            metadata_location: None,
            edge_override: None,
            layout_override: None,
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

    /// A YAML header is written back as YAML: the shape of front matter belongs to the
    /// file it is in, not to the caller changing a key in it.
    #[test]
    fn test_live_entity_yaml_write() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "entity1.md", "---\nkey: val\n---\nHello");
        let fs = Arc::new(Mutex::new(fs));
        let schema = setup_schema();

        let live = LiveEntity::new(
            fs.clone(),
            PathBuf::from("foo"),
            EntityPath::empty().extend_slash("entity1"),
            "Type".to_string(),
            schema,
        );

        live.set_meta_key("key", toml::Value::String("other".to_string())).unwrap();

        let content = crate::entity::utils::try_load_file_as_string(&*live.root.fs.lock().unwrap(), &PathBuf::from("foo/entity1.md")).unwrap().unwrap();
        assert!(content.contains("---\nkey: other\n---\n"), "got: {:?}", content);
        assert!(content.ends_with("Hello"), "the body survives: {:?}", content);
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

        // 1. set_content. Nothing is established here, so intent decides: `Type` declares
        // no layout and this handle was built by address, so it inherits `Inside`. This
        // used to come out `Parallel`, from a heuristic that read the absence of a
        // directory as a decision the node had made.
        live.set_content("New content").unwrap();
        assert_eq!(live.content().unwrap(), EntityContent::inside("New content"));

        // 2. set_metadata_at (Inside)
        let meta = crate::entity::Metadata { value: toml::from_str("a = 1").unwrap() };
        live.set_metadata_at(MetaLocation::InsideSidecar, meta.clone()).unwrap();
        assert_eq!(live.metadata().unwrap(), EntityMeta::inside(live.path.clone(), meta));

        // 3. create_child
        live.create_child("child1")
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

        live.clear_metadata().unwrap();

        assert!(live.metadata().unwrap().is_none());
        assert!(
            !live.root.fs.lock().unwrap().is_file(&PathBuf::from("foo/entity1.meta.toml")),
            "the sidecar is gone, not merely emptied"
        );
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

        live.create_child("child")
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
            .create_child("child")
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
            .create_child("notes").with_edge(Edge::Dot)
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
            .create_child("child_one")
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
            .create_child("other")
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
            .create_child("child_one")
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
            .create_child("anything")
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
            .create_child("item")
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
            .create_child("thing")
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
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        // Deliberately construct a non-Table Metadata value
        let bad_meta = Metadata { value: toml::Value::String("not a table".to_string()) };
        let err = live
            .create_child("item")
            .with_type("Chapter")
            .with_metadata_at(MetaLocation::InsideSidecar, bad_meta)
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

        live.create_child("child")
            .with_content("Hello Inside")
            .build()
            .unwrap();

        let content = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent/child/content.md"),
        ).unwrap().unwrap();
        assert_eq!(content, "Hello Inside");
    }

    /// C3: a dot child is parallel because its *layout* says so, not because of its edge.
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

        live.create_child("notes").with_edge(Edge::Dot)
            .with_layout(Layout::Parallel)
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

        live.create_child("notes").with_edge(Edge::Dot)
            .with_layout(Layout::Inside).with_content("Forced Inside")
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

        live.create_child("child")
            .with_layout(Layout::Parallel).with_content("Forced Parallel")
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
        live.create_child("child")
            .with_metadata_at(MetaLocation::InsideSidecar, meta)
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
        live.create_child("notes").with_edge(Edge::Dot)
            .with_metadata_at(MetaLocation::ParallelSidecar, meta)
            .build()
            .unwrap();

        let raw = crate::entity::utils::try_load_file_as_string(
            &*fs.lock().unwrap(),
            &PathBuf::from("foo/parent.notes.meta.toml"),
        ).unwrap().unwrap();
        assert!(raw.contains("note"), "got: {}", raw);
    }

    /// §6: a parallel node with nothing to write leaves no trace on disk, so it is
    /// refused. An inside node would at least be a directory, so it is not.
    #[test]
    fn test_parallel_child_with_no_content_metadata_or_children_errors() {
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
            .create_child("notes").with_edge(Edge::Dot)
            .with_layout(Layout::Parallel)
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
            .create_child("notes").with_edge(Edge::Dot)
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
            fs.clone(), PathBuf::from("foo"),
            EntityPath::empty().extend_slash("root"),
            "Parent".to_string(), schema,
        );

        let meta = Metadata {
            value: toml::from_str("title = \"My Chapter\"").unwrap(),
        };
        live.create_child("item")
            .with_type("Chapter")
            .with_metadata_at(MetaLocation::InsideSidecar, meta)
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
            .create_child("child")
            .with_metadata_at(MetaLocation::InHeader, meta)
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

        live.create_child("chapter")
            .with_child("scene", |b| {
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
            .create_child("ch_one")
            .with_child("sc_one", |b| b)
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
            .create_child("thing")
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
    pub(super) const SCHEMA: &str = r#"
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

#[cfg(test)]
mod live_write_tests {
    use super::live_read_tests::SCHEMA;
    use super::*;
    use inscenerator_xfs::mockfs;
    use std::collections::BTreeMap;
    use std::path::Path;
    use std::sync::{Arc, Mutex};

    /// A content file carrying front matter, so a node can have metadata in two places
    /// at once. Round-trips exactly: the body below the `---` is `body`.
    const HEADER: &str = "```toml\nowner = \"hand\"\n```\n---\nbody";

    /// The files on disk before, and the one file the write is expected to touch.
    type ContentCase<'a> = (&'a [(&'a str, &'a str)], &'a str, &'a str);
    /// As [`ContentCase`], with the key being written.
    type MetaCase<'a> = (&'a [(&'a str, &'a str)], &'a str, &'a str, &'a str);

    fn meta(src: &str) -> Metadata {
        Metadata { value: toml::from_str(src).unwrap() }
    }

    /// A handle on `ch1`, typed `Chapter` — which declares `layout = "parallel"`, so
    /// intent and what is on disk can be made to disagree. Built by address rather than
    /// through `child()`: these tests are about where a write lands, not about discovery.
    fn ch1_handle(files: &[(&str, &str)]) -> LiveEntity {
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
            EntityPath::empty().extend_slash("ch1"),
            "Chapter".to_string(),
            Arc::new(Schema::load_from_str(SCHEMA).unwrap()),
        )
    }

    /// Every file under the tree, with its contents.
    fn snapshot(live: &LiveEntity) -> BTreeMap<String, String> {
        fn walk(fs: &dyn Xfs, dir: &Path, out: &mut BTreeMap<String, String>) {
            let entries: Vec<PathBuf> =
                fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
            for p in entries {
                if fs.is_dir(&p) {
                    walk(fs, &p, out);
                } else {
                    let text = utils::try_load_file_as_string(fs, &p).unwrap().unwrap_or_default();
                    out.insert(p.display().to_string(), text);
                }
            }
        }
        let fs = live.root.fs.lock().unwrap();
        let mut out = BTreeMap::new();
        walk(&*fs, &live.root.base_path, &mut out);
        out
    }

    /// The files `f` created, deleted or rewrote. Most of the assertions below are about
    /// this list being *short*: a write routes to one file and leaves the rest alone.
    fn files_changed_by(live: &LiveEntity, f: impl FnOnce(&LiveEntity)) -> Vec<String> {
        let before = snapshot(live);
        f(live);
        let after = snapshot(live);
        let mut paths: Vec<String> = before.keys().chain(after.keys()).cloned().collect();
        paths.sort();
        paths.dedup();
        paths.retain(|p| before.get(p) != after.get(p));
        paths
    }

    /// §4.3: content is written where the node already keeps its content, and only a
    /// node with none falls back to what its type intends.
    #[test]
    fn set_content_lands_where_4_3_says() {
        let cases: &[ContentCase] = &[
            (&[], "foo/ch1.md", "nothing is established, so the type's intent decides"),
            (
                &[("foo/ch1/content.md", "old")],
                "foo/ch1/content.md",
                "content already lives inside, and a write is not a relocation",
            ),
            (&[("foo/ch1.md", "old")], "foo/ch1.md", "content already lives beside"),
        ];
        for (files, expected, why) in cases {
            let ch1 = ch1_handle(files);
            let changed = files_changed_by(&ch1, |e| e.set_content("new").unwrap());
            assert_eq!(changed, vec![expected.to_string()], "{}", why);
            assert_eq!(ch1.content().unwrap(), "new", "{}", why);
        }
    }

    /// §4.3: a content write must not destroy the metadata that shares its file.
    #[test]
    fn set_content_keeps_the_header_above_it() {
        let ch1 = ch1_handle(&[("foo/ch1.md", HEADER)]);
        ch1.set_content("new body").unwrap();

        assert_eq!(ch1.content().unwrap(), "new body");
        assert_eq!(ch1.metadata().unwrap().get_str("owner").unwrap().as_deref(), Some("hand"));
    }

    /// §4.5: one key is written to one source, chosen by where that key already lives,
    /// else the node's only source, else the type's intent. Nothing else moves — which
    /// is what `changed` proves in each case.
    #[test]
    fn set_meta_key_lands_where_4_5_says() {
        let sidecar = ("foo/ch1.meta.toml", "word_count = 1\n");
        let inside_sidecar = ("foo/ch1/meta.toml", "word_count = 1\n");
        let cases: &[MetaCase] = &[
            (
                &[("foo/ch1.md", HEADER)],
                "owner",
                "foo/ch1.md",
                "the source that already holds the key",
            ),
            (
                &[("foo/ch1.md", HEADER), sidecar],
                "word_count",
                "foo/ch1.meta.toml",
                "the source that holds the key, not the first source that exists",
            ),
            (
                &[("foo/ch1.md", HEADER), sidecar],
                "added",
                "foo/ch1.meta.toml",
                "a new key on a node with two sources follows intent",
            ),
            (
                &[("foo/ch1.md", "body"), inside_sidecar],
                "added",
                "foo/ch1/meta.toml",
                "C7: the node's only source wins over intent, so a mixed node is not normalised",
            ),
            (
                &[("foo/ch1.md", "body")],
                "added",
                "foo/ch1.meta.toml",
                "no metadata at all, so intent — the parallel sidecar — and it is created",
            ),
            (
                &[("foo/ch1.md", HEADER)],
                "added",
                "foo/ch1.md",
                "the header is the node's only source, so a new key joins it",
            ),
        ];
        for (files, key, expected, why) in cases {
            let ch1 = ch1_handle(files);
            let changed =
                files_changed_by(&ch1, |e| e.set_meta_key(key, toml::Value::Integer(7)).unwrap());
            assert_eq!(changed, vec![expected.to_string()], "{}", why);

            let merged = ch1.metadata().unwrap().merged().unwrap().unwrap();
            assert_eq!(merged.value.get(*key).unwrap().as_integer(), Some(7), "{}", why);
        }
    }

    /// §4.5: routing a key to the header rewrites the front matter and nothing else —
    /// in particular not the body underneath it.
    #[test]
    fn setting_a_key_in_the_header_leaves_the_body_alone() {
        let ch1 = ch1_handle(&[("foo/ch1.md", HEADER)]);
        ch1.set_meta_key("owner", toml::Value::String("robot".into())).unwrap();

        assert_eq!(ch1.content().unwrap(), "body");
        assert_eq!(ch1.metadata().unwrap().get_str("owner").unwrap().as_deref(), Some("robot"));
    }

    /// §4.5: removal is routed exactly as setting is — from the source holding the key.
    #[test]
    fn removing_a_key_takes_it_out_of_the_source_that_holds_it() {
        let ch1 = ch1_handle(&[("foo/ch1.md", HEADER), ("foo/ch1.meta.toml", "word_count = 1\n")]);
        let changed = files_changed_by(&ch1, |e| e.remove_meta_key("word_count").unwrap());
        assert_eq!(changed, vec!["foo/ch1.meta.toml".to_string()]);

        let m = ch1.metadata().unwrap();
        assert_eq!(m.get_str("owner").unwrap().as_deref(), Some("hand"), "the header is untouched");
        assert!(m.merged().unwrap().unwrap().value.get("word_count").is_none());
        assert_eq!(
            ch1.observed().unwrap().metadata,
            vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar],
            "an emptied sidecar stays: removing a key is not removing a source"
        );
    }

    /// §4.5: the explicit route, for a caller replacing one whole source on purpose.
    #[test]
    fn set_metadata_at_replaces_one_source_and_leaves_the_others() {
        let ch1 = ch1_handle(&[("foo/ch1.md", HEADER), ("foo/ch1.meta.toml", "word_count = 1\n")]);
        let changed = files_changed_by(&ch1, |e| {
            e.set_metadata_at(MetaLocation::ParallelSidecar, meta("stage = \"draft\"")).unwrap()
        });
        assert_eq!(changed, vec!["foo/ch1.meta.toml".to_string()]);

        let m = ch1.metadata().unwrap();
        assert_eq!(m.get_str("stage").unwrap().as_deref(), Some("draft"));
        assert_eq!(m.get_str("owner").unwrap().as_deref(), Some("hand"), "the header is untouched");
        let merged = m.merged().unwrap().unwrap();
        assert!(merged.value.get("word_count").is_none(), "a replacement, not a merge");
    }

    /// §4.5: clearing removes every source, front matter included. The content is not
    /// metadata, so it survives having the header stripped off it.
    #[test]
    fn clear_metadata_removes_every_source_and_keeps_the_body() {
        let ch1 = ch1_handle(&[("foo/ch1.md", HEADER), ("foo/ch1.meta.toml", "word_count = 1\n")]);
        ch1.clear_metadata().unwrap();

        assert!(ch1.metadata().unwrap().is_none());
        assert_eq!(ch1.observed().unwrap().metadata, vec![]);
        assert_eq!(ch1.content().unwrap(), "body");
    }

    /// D3: a source the library could not parse is still replaceable wholesale — that is
    /// how a caller repairs one.
    #[test]
    fn a_malformed_source_is_repairable_by_wholesale_replacement() {
        let ch1 =
            ch1_handle(&[("foo/ch1.md", "body"), ("foo/ch1.meta.toml", "this = = not toml")]);
        ch1.set_metadata_at(MetaLocation::ParallelSidecar, meta("word_count = 3")).unwrap();

        let m = ch1.metadata().unwrap();
        assert!(m.malformed().is_empty());
        assert_eq!(
            m.merged().unwrap().unwrap().value.get("word_count").unwrap().as_integer(),
            Some(3)
        );
    }

    /// D3: and it is removable, which is the other way to get rid of one.
    #[test]
    fn clear_metadata_removes_a_malformed_source() {
        let ch1 =
            ch1_handle(&[("foo/ch1.md", "body"), ("foo/ch1.meta.toml", "this = = not toml")]);
        ch1.clear_metadata().unwrap();

        assert!(ch1.metadata().unwrap().is_none());
    }

    /// D3: but the library will not merge a key into text it could not parse — that
    /// would mean writing back a file whose contents it does not understand. The refusal
    /// names the file, because that is what the person fixing it has to open; a Rust
    /// method name would be noise in the tooling these errors surface through.
    #[test]
    fn set_meta_key_refuses_to_edit_a_source_it_could_not_parse() {
        let ch1 =
            ch1_handle(&[("foo/ch1.md", "body"), ("foo/ch1.meta.toml", "this = = not toml")]);
        let err = ch1.set_meta_key("added", toml::Value::Integer(1)).unwrap_err().to_string();

        assert!(err.contains("foo/ch1.meta.toml"), "the message names the file: {}", err);
        assert!(!err.contains("set_metadata_at"), "no Rust API in the message: {}", err);
    }
}

#[cfg(test)]
pub(super) mod create_child_tests {
    use super::*;
    use crate::placement::Edge;
    use inscenerator_xfs::mockfs;
    use std::path::Path;
    use std::sync::{Arc, Mutex};

    /// `Chapter` is parallel; `Section` declares no layout, so it takes the layout of the
    /// chapter instance it is created under (§2.1). `Figure` declares its own. The three
    /// `Chapter` rules cover both edges, which is what edge resolution is read against.
    pub(super) const SCHEMA: &str = r#"
[Root]
allow_additional = false
[[Root.children]]
name_regex = "^ch"
node_type = "Chapter"

[Chapter]
allow_additional = false
layout = "parallel"
[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"
[[Chapter.children]]
name_regex = "^review$"
node_type = "Section"
edge = "dot"
[[Chapter.children]]
name_regex = "^fig-"
node_type = "Figure"
edge = "slash"

[Section]
allow_additional = false
children = []

[Figure]
allow_additional = false
layout = "inside"
children = []
"#;

    /// A case: the child to create, and the whole tree that must result.
    type BuildCase<'a> = (&'a str, &'a [&'a str], &'a str);

    pub(super) fn fs_with(files: &[(&str, &str)]) -> Arc<Mutex<mockfs::MockFS>> {
        let mut fs = mockfs::MockFS::new();
        fs.create_dir_all(&PathBuf::from("foo")).unwrap();
        for (path, content) in files {
            let p = PathBuf::from(path);
            fs.create_dir_all(p.parent().unwrap()).unwrap();
            fs.add_r(&p, content.as_bytes().to_vec()).unwrap();
        }
        Arc::new(Mutex::new(fs))
    }

    /// A handle on the chapter `ch1`, reached through the root so it inherits normally.
    fn ch1(files: &[(&str, &str)]) -> LiveEntity {
        LiveEntity::new(
            fs_with(files),
            PathBuf::from("foo"),
            EntityPath::empty(),
            "Root".to_string(),
            Arc::new(Schema::load_from_str(SCHEMA).unwrap()),
        )
        .child("ch1")
        .unwrap()
    }

    /// Everything under the base path, directories included and marked with a trailing
    /// `/`. These tests assert the *whole* tree, because what was not created — a
    /// directory for a node that is a file, a `content.md` beside a spine file — is as
    /// much of the claim as what was.
    pub(super) fn tree(live: &LiveEntity) -> Vec<String> {
        fn walk(fs: &dyn Xfs, dir: &Path, out: &mut Vec<String>) {
            let mut entries: Vec<PathBuf> =
                fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
            entries.sort();
            for p in entries {
                if fs.is_dir(&p) {
                    out.push(format!("{}/", p.display()));
                    walk(fs, &p, out);
                } else {
                    out.push(p.display().to_string());
                }
            }
        }
        let fs = live.root.fs.lock().unwrap();
        let mut out = Vec::new();
        walk(&*fs, &live.root.base_path, &mut out);
        out
    }

    fn meta(src: &str) -> Metadata {
        Metadata { value: toml::from_str(src).unwrap() }
    }

    /// §5: the shape the whole design exists to make expressible — a readable spine file
    /// with a folder of its sections beside it — built through the API by nothing but
    /// names. Neither the edge nor the layout is stated at any call site here; both come
    /// out of the schema.
    #[test]
    fn section_five_shape_is_buildable_through_the_api() {
        const BOOK: &str = r#"
[Root]
allow_additional = false
[[Root.children]]
name_regex = "^chapters$"
node_type = "Chapters"

[Chapters]
allow_additional = false
layout = "inside"
[[Chapters.children]]
name_regex = '^\d{3}-'
node_type = "Chapter"

[Chapter]
allow_additional = false
layout = "parallel"
[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
[[Chapter.children]]
name_regex = "^review$"
node_type = "Section"

[Section]
allow_additional = false
children = []
"#;
        let fs = fs_with(&[("book/meta.toml", "type = \"Root\"")]);
        let root = LiveEntity::load_from_root_with(
            fs,
            PathBuf::from("book"),
            Arc::new(Schema::load_from_str(BOOK).unwrap()),
        )
        .unwrap();

        let chapters = root.create_child("chapters").build().unwrap();
        let ch = chapters
            .create_child("000-the-invisible-kitchen")
            .with_content("Chapter body")
            .build()
            .unwrap();
        ch.create_child("010-what-fermentation-is").with_content("Section body").build().unwrap();
        ch.create_child("review").with_content("Review body").build().unwrap();

        assert_eq!(
            tree(&root),
            vec![
                "book/chapters/",
                "book/chapters/000-the-invisible-kitchen/",
                "book/chapters/000-the-invisible-kitchen/010-what-fermentation-is.md",
                "book/chapters/000-the-invisible-kitchen/review.md",
                "book/chapters/000-the-invisible-kitchen.md",
                "book/meta.toml",
            ]
        );
    }

    /// C4: the edge a new child attaches on is declared by the parent's rule, not implied
    /// by anything the caller says.
    #[test]
    fn a_new_child_goes_on_the_edge_its_rule_declares() {
        let cases: &[BuildCase] = &[
            ("010-intro", &["foo/ch1/", "foo/ch1/010-intro.md", "foo/ch1.md"], "edge = slash"),
            ("review", &["foo/ch1.md", "foo/ch1.review.md"], "edge = dot"),
        ];
        for (name, expected, why) in cases {
            let ch1 = ch1(&[("foo/ch1.md", "chapter")]);
            ch1.create_child(name).with_content("body").build().unwrap();
            assert_eq!(tree(&ch1), *expected, "{}", why);
        }
    }

    /// §4.2: a rule declares an edge, but children already sitting on the other one are
    /// evidence of a decision this tree has made. A new sibling joins them rather than
    /// splitting the rule in two.
    #[test]
    fn a_new_child_joins_the_edge_its_siblings_are_on() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter"), ("foo/ch1.010-intro.md", "intro")]);
        ch1.create_child("020-body").with_content("body").build().unwrap();

        assert_eq!(
            tree(&ch1),
            vec!["foo/ch1.010-intro.md", "foo/ch1.020-body.md", "foo/ch1.md"]
        );
    }

    /// §4.5: but a rule already split across both edges is not evidence of anything, so a
    /// new child conforms to the declaration rather than spreading the split further.
    #[test]
    fn a_new_child_under_a_split_rule_takes_the_declared_edge() {
        let ch1 = ch1(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1.010-intro.md", "intro"),
            ("foo/ch1/020-body.md", "body"),
        ]);
        ch1.create_child("030-end").with_content("end").build().unwrap();

        assert_eq!(
            tree(&ch1),
            vec![
                "foo/ch1/",
                "foo/ch1/020-body.md",
                "foo/ch1/030-end.md",
                "foo/ch1.010-intro.md",
                "foo/ch1.md",
            ]
        );
    }

    /// §2.1 / §8.1: layout is the child type's when it declares one, and the layout of the
    /// parent instance otherwise — which is what makes a section beside a chapter file a
    /// file too, without either type having to mention the other.
    #[test]
    fn a_childs_layout_is_its_types_or_the_parent_instances() {
        let cases: &[BuildCase] = &[
            (
                "010-intro",
                &["foo/ch1/", "foo/ch1/010-intro.md", "foo/ch1.md"],
                "Section declares no layout, so it inherits the chapter's",
            ),
            (
                "fig-1",
                &["foo/ch1/", "foo/ch1/fig-1/", "foo/ch1/fig-1/content.md", "foo/ch1.md"],
                "Figure declares inside, which beats what it would have inherited",
            ),
        ];
        for (name, expected, why) in cases {
            let ch1 = ch1(&[("foo/ch1.md", "chapter")]);
            ch1.create_child(name).with_content("body").build().unwrap();
            assert_eq!(tree(&ch1), *expected, "{}", why);
        }
    }

    /// §8.1: the two are separately overridable, which is the point of keeping them
    /// orthogonal — and the escape hatch, not the ordinary path. Each override is applied
    /// to its own tree, since one child is evidence the next one would follow (§4.2).
    #[test]
    fn with_edge_and_with_layout_override_what_was_resolved() {
        let dotted = ch1(&[("foo/ch1.md", "chapter")]);
        dotted.create_child("010-intro").with_content("body").with_edge(Edge::Dot).build().unwrap();
        assert_eq!(tree(&dotted), vec!["foo/ch1.010-intro.md", "foo/ch1.md"]);

        let ch1b = ch1(&[("foo/ch1.md", "chapter")]);
        ch1b.create_child("010-intro")
            .with_content("body")
            .with_layout(Layout::Inside)
            .build()
            .unwrap();
        assert_eq!(
            tree(&ch1b),
            vec!["foo/ch1/", "foo/ch1/010-intro/", "foo/ch1/010-intro/content.md", "foo/ch1.md"]
        );
    }

    /// §4.1: a new child's metadata goes where its layout says, and nowhere else unless
    /// the caller places it. C3: the edge it hangs off has no say in this.
    #[test]
    fn metadata_lands_in_the_layouts_sidecar_unless_placed_explicitly() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter")]);
        ch1.create_child("010-intro").with_metadata(meta("owner = \"a\"")).build().unwrap();
        ch1.create_child("review")
            .with_content("body")
            .with_metadata_at(MetaLocation::InHeader, meta("owner = \"b\""))
            .build()
            .unwrap();

        assert_eq!(
            tree(&ch1),
            vec![
                "foo/ch1/",
                "foo/ch1/010-intro.meta.toml",
                "foo/ch1.md",
                "foo/ch1.review.md",
            ],
            "the parallel sidecar for one, the content file itself for the other"
        );
        let owner = |name: &str| ch1.child(name).unwrap().metadata().unwrap().get_str("owner").unwrap();
        assert_eq!(owner("010-intro").as_deref(), Some("a"));
        assert_eq!(owner("review").as_deref(), Some("b"));
    }

    /// The handle `build` hands back is a handle on the child it just wrote: it reads
    /// that child's content, and children created through it inherit its layout rather
    /// than the layout its edge would have implied.
    #[test]
    fn the_handle_build_returns_reads_the_child_it_just_wrote() {
        let ch1 = ch1(&[("foo/ch1.md", "chapter")]);
        let intro = ch1.create_child("010-intro").with_content("intro body").build().unwrap();

        assert_eq!(intro.content().unwrap(), "intro body");
        assert_eq!(intro.intended_layout().unwrap(), Layout::Parallel);
        assert_eq!(intro.actual_type().unwrap(), "Section");
    }
}

/// `move_to` relocates; it never normalises. Section references are to
/// `docs/storage-layout-v2.md`.
#[cfg(test)]
mod move_tests {
    use super::create_child_tests::{fs_with, tree, SCHEMA};
    use super::*;
    use crate::findings::FindingKind;
    use std::sync::Arc;

    /// A handle on the root of a tree built from `files`.
    fn root(files: &[(&str, &str)]) -> LiveEntity {
        LiveEntity::new(
            fs_with(files),
            PathBuf::from("foo"),
            EntityPath::empty(),
            "Root".to_string(),
            Arc::new(Schema::load_from_str(SCHEMA).unwrap()),
        )
    }

    /// C1's last site: the sidecar suffix is appended to the new stem, so a moved
    /// dot-child cannot land on its parent's sidecar.
    #[test]
    fn move_relocates_a_dot_childs_appended_sidecar() {
        let root = root(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1.meta.toml", "owner = \"chapter\""),
            ("foo/ch1.review.md", "review"),
            ("foo/ch1.review.meta.toml", "owner = \"review\""),
        ]);
        let ch1 = root.child("ch1").unwrap();
        let mut review = ch1.child("review").unwrap();

        review.move_to(ch1.path.extend_dot("notes")).unwrap();

        assert_eq!(
            tree(&root),
            vec![
                "foo/ch1.md",
                "foo/ch1.meta.toml",
                "foo/ch1.notes.md",
                "foo/ch1.notes.meta.toml",
            ]
        );
        // The parent's own metadata is untouched, which is the whole of C1.
        assert_eq!(ch1.metadata().unwrap().get_str("owner").unwrap().as_deref(), Some("chapter"));
    }

    /// §9.3: a dot-descendant's path derives from this node's, so relocating this node
    /// drags it along.
    #[test]
    fn move_drags_dot_descendants() {
        let root = root(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1.review.md", "review"),
            ("foo/ch1.review.draft.md", "draft"),
        ]);
        let ch1 = root.child("ch1").unwrap();
        let mut review = ch1.child("review").unwrap();

        review.move_to(ch1.path.extend_dot("notes")).unwrap();

        assert_eq!(
            tree(&root),
            vec!["foo/ch1.md", "foo/ch1.notes.draft.md", "foo/ch1.notes.md"]
        );
    }

    /// D5: layout survives a move. `fig-1` matches a rule whose type declares `inside`,
    /// but the files that arrived are parallel, so they stay parallel and the resulting
    /// disagreement is reported rather than repaired.
    #[test]
    fn move_preserves_layout_and_reports_the_result() {
        let root = root(&[("foo/ch1.md", "chapter"), ("foo/ch1/010-intro.md", "intro")]);
        let ch1 = root.child("ch1").unwrap();
        let mut intro = ch1.child("010-intro").unwrap();

        intro.move_to(ch1.path.extend_slash("fig-1")).unwrap();

        assert_eq!(tree(&root), vec!["foo/ch1/", "foo/ch1/fig-1.md", "foo/ch1.md"]);

        let fig = ch1.child("fig-1").unwrap();
        assert_eq!(fig.actual_type().unwrap(), "Figure");
        assert_eq!(fig.intended_layout().unwrap(), Layout::Inside);
        assert!(fig.issues().unwrap().iter().any(|f| matches!(
            f.kind,
            FindingKind::ContentLocationNonconformance {
                actual: ContentLocation::Parallel,
                intended: ContentLocation::Inside,
            }
        )));
    }

    /// C7: a mixed node keeps both of its files. The directory rename carries the inside
    /// sidecar; the parallel content has to be moved on its own.
    #[test]
    fn move_carries_both_halves_of_a_mixed_node() {
        let root = root(&[
            ("foo/ch1.md", "chapter"),
            ("foo/ch1/010-intro.md", "intro"),
            ("foo/ch1/010-intro/meta.toml", "owner = \"intro\""),
        ]);
        let ch1 = root.child("ch1").unwrap();
        let mut intro = ch1.child("010-intro").unwrap();

        intro.move_to(ch1.path.extend_slash("020-body")).unwrap();

        assert_eq!(
            tree(&root),
            vec![
                "foo/ch1/",
                "foo/ch1/020-body/",
                "foo/ch1/020-body/meta.toml",
                "foo/ch1/020-body.md",
                "foo/ch1.md",
            ]
        );
    }

    #[test]
    fn moving_a_node_that_is_not_there_is_an_error() {
        let root = root(&[("foo/ch1.md", "chapter")]);
        let mut ghost = root.child("ch1").unwrap();
        ghost.path = EntityPath::empty().extend_slash("ch9");
        let err = ghost.move_to(EntityPath::empty().extend_slash("ch8")).unwrap_err();
        assert!(err.to_string().contains("Nothing found to move"), "got: {}", err);
    }
}
