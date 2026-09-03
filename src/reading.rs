//! One node, read from disk: its type, its layout, its content and its metadata.
//!
//! Section references are to `docs/storage-layout-v2.md`.
//!
//! [`read_node`] is the single implementation of §4.1 and §4.4 — which of the two content
//! files wins, which metadata sources exist, and what the node's layout turns out to be.
//! Both [`crate::entity::EntityLoader`], which walks a whole tree eagerly, and
//! [`crate::live_entity::LiveEntity`], which reads one node at a time, resolve through
//! here. That shared path is what makes them agree; two implementations of it is defect
//! C2, and the reason C5 exists.
//!
//! Nothing here descends. Children are [`crate::discovery`]'s subject, and the callers
//! differ on what they do with them.

use std::path::Path;

use anyhow::{anyhow, bail};
use inscenerator_xfs::Xfs;

use crate::entity::{utils, EntityContent, EntityMeta, EntityPath, MetaOrigin};
use crate::findings::{Finding, FindingKind, FindingSink};
use crate::placement::{self, ContentLocation, Edge, Layout, MetaLocation};
use crate::schema::Schema;

/// What one node turned out to be. Drift met on the way is reported to the sink.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeRead {
    /// The type this node actually is, with `Auto` resolved. §7.1.
    pub actual_type: String,
    /// The layout it was resolved under: declared by its type, or inherited from the
    /// parent instance it was read beneath. §2.1.
    pub layout: Layout,
    pub content: EntityContent,
    pub metadata: EntityMeta,
    /// Whether the stem directory exists. Part of §6's "is there a node here at all",
    /// which the caller decides.
    pub directory_exists: bool,
}

impl NodeRead {
    /// §6: a node with no content, no metadata and no directory is not a node — unless
    /// it has children, which the caller knows about and this module does not.
    pub fn is_absent(&self) -> bool {
        self.content.is_none() && self.metadata.is_none() && !self.directory_exists
    }
}

/// Reads one node.
///
/// `entity_type` is what the parent's rule assigned, or `"Auto"` to take the type from
/// the node's own metadata. `inherited_layout` is the layout of the parent *instance*,
/// used when the node's type declares none (§2.1).
///
/// # Errors
///
/// Returns an error for a schema that cannot describe this node at all — an unknown
/// type, an `Auto` node with no type recorded, a root type declaring a layout or an edge
/// the root cannot have — and for any finding the policy rates `Error`. Ordinary drift
/// is reported to `sink` and does not fail the read.
pub fn read_node(
    fs: &dyn Xfs,
    schema: &Schema,
    base_path: &Path,
    path: &EntityPath,
    entity_type: &str,
    inherited_layout: Layout,
    sink: &mut FindingSink,
) -> anyhow::Result<NodeRead> {
    let is_root = path.entries.is_empty();
    let at = |kind: FindingKind| Finding { path: path.clone(), kind };

    // ---- Probe. Existence only: which file *wins* cannot be decided until the type is
    // known, and the type may itself be recorded in one of these files.
    let directory_exists = fs.is_dir(&placement::stem(base_path, path));
    let parallel_content = placement::content_path(base_path, path, ContentLocation::Parallel);
    let inside_content = placement::content_path(base_path, path, ContentLocation::Inside);
    // The root has no filename to hang a parallel file off, so only `Inside` exists.
    let has_parallel_content = !is_root && fs.is_file(&parallel_content);
    let has_inside_content = fs.is_file(&inside_content);

    // ---- Sidecars. Both are read; a file that does not parse is kept rather than being
    // allowed to abort the read (D3).
    let mut meta_sources = Vec::new();
    for location in [MetaLocation::ParallelSidecar, MetaLocation::InsideSidecar] {
        // The root has no parallel sidecar, for the same reason it has no parallel content.
        if is_root && location == MetaLocation::ParallelSidecar {
            continue;
        }
        let origin = match location {
            MetaLocation::ParallelSidecar => MetaOrigin::ParallelSidecar,
            MetaLocation::InsideSidecar => MetaOrigin::InsideSidecar,
            MetaLocation::InHeader => unreachable!("not a sidecar"),
        };
        let file = placement::sidecar_path(base_path, path, location)
            .expect("a sidecar location always has a path");
        if let Some(source) = utils::try_load_sidecar(fs, &file, origin, path)? {
            meta_sources.push(source);
        }
    }

    // ---- Type, then layout, then the content choice.
    //
    // Choosing between two content files needs the intended layout, which needs the type,
    // which may itself live in the losing file's front matter. Where both files exist that
    // is genuinely circular, so the type is resolved from the sidecars alone; where only
    // one exists there is nothing to choose and its header takes part in typing normally,
    // since it is read before the type is needed.
    let forced_content = match (has_parallel_content, has_inside_content) {
        (true, false) => Some(&parallel_content),
        (false, true) => Some(&inside_content),
        _ => None,
    };
    let early_header = match forced_content {
        Some(file) => utils::try_load_file_as_string(fs, file)?
            .and_then(|c| utils::parse_header_source(&c, path))
            .map(|(source, _)| source),
        None => None,
    };
    let typing_meta = match &early_header {
        Some(h) => {
            let mut v = meta_sources.clone();
            v.push(h.clone());
            EntityMeta::of(v)
        }
        None => EntityMeta::of(meta_sources.clone()),
    };

    let actual_type = resolve_type(path, entity_type, &typing_meta, sink)?;
    let layout = resolve_layout(schema, &actual_type, is_root, inherited_layout)?;
    let intended_content = layout.content_location();

    // ---- Read the content, now that intent can pick.
    let chosen = match (has_parallel_content, has_inside_content) {
        (false, false) => None,
        (true, true) => {
            // §4.4: intent picks, and the file that lost is reported. Neither is deleted;
            // both were previously a hard error.
            let (keep, lost, lost_path) = match intended_content {
                ContentLocation::Parallel => {
                    (ContentLocation::Parallel, ContentLocation::Inside, &inside_content)
                }
                ContentLocation::Inside => {
                    (ContentLocation::Inside, ContentLocation::Parallel, &parallel_content)
                }
            };
            sink.report(at(FindingKind::StrayContent {
                path: lost_path.clone(),
                location: lost,
            }))?;
            Some(keep)
        }
        (true, false) => Some(ContentLocation::Parallel),
        (false, true) => Some(ContentLocation::Inside),
    };
    if let Some(actual) = chosen {
        if actual != intended_content {
            sink.report(at(FindingKind::ContentLocationNonconformance {
                actual,
                intended: intended_content,
            }))?;
        }
    }

    let (content, header_source) = match chosen {
        None => (EntityContent::None, None),
        Some(loc) => {
            let file = match loc {
                ContentLocation::Parallel => &parallel_content,
                ContentLocation::Inside => &inside_content,
            };
            let raw = utils::try_load_file_as_string(fs, file)?.unwrap_or_default();
            let (source, body) = match utils::parse_header_source(&raw, path) {
                Some((source, body)) => (Some(source), body),
                None => (None, raw),
            };
            let content = match loc {
                ContentLocation::Parallel => EntityContent::Parallel(body),
                ContentLocation::Inside => EntityContent::Inside(body),
            };
            (content, source)
        }
    };
    if let Some(h) = header_source {
        meta_sources.push(h);
    }
    let metadata = EntityMeta::of(meta_sources);

    // ---- Report what the metadata turned out to be.
    for source in metadata.malformed() {
        sink.report(at(FindingKind::MalformedMetadata {
            location: source.location(),
            path: placement::sidecar_path(base_path, path, source.location()),
            error: source.error().unwrap_or("unknown error").to_string(),
        }))?;
    }
    for conflict in metadata.conflicts() {
        sink.report(at(FindingKind::MetadataKeyConflict {
            key: conflict.key,
            locations: conflict.locations,
        }))?;
    }
    if metadata.sources().len() > 1 {
        sink.report(at(FindingKind::SplitMetadata {
            keys_by_location: metadata.keys_by_location(),
        }))?;
    }
    for source in metadata.sources() {
        // In-header metadata is orthogonal to layout (D2), so it is never misplaced.
        let actual = source.location();
        if actual != MetaLocation::InHeader && actual != layout.sidecar_location() {
            sink.report(at(FindingKind::MetadataLocationNonconformance {
                actual,
                intended: layout.sidecar_location(),
            }))?;
        }
    }

    Ok(NodeRead { actual_type, layout, content, metadata, directory_exists })
}

/// The layout a node of this type resolves to. §2.1.
///
/// # Errors
///
/// Returns an error if the type is not in the schema, or if it is the root's type and
/// declares something the root cannot be.
pub fn resolve_layout(
    schema: &Schema,
    actual_type: &str,
    is_root: bool,
    inherited_layout: Layout,
) -> anyhow::Result<Layout> {
    let ctype = schema.compiled(actual_type)?;
    if !is_root {
        return Ok(ctype.desc.layout.unwrap_or(inherited_layout));
    }
    // §2 and §3 as schema errors rather than drift: no tree can be shaped this way, so
    // there is nothing on disk to tolerate.
    if ctype.desc.layout == Some(Layout::Parallel) {
        bail!(
            "Root type '{}' declares layout = \"parallel\", but the root is always \
             inside: it has no parent filename to sit beside",
            actual_type
        );
    }
    if let Some(r) = ctype.rules.iter().find(|r| r.rule.edge == Edge::Dot) {
        bail!(
            "Root type '{}' declares edge = \"dot\" for '{}', but the root has no \
             dot children: there is no filename to prefix",
            actual_type,
            r.rule.name_regex
        );
    }
    Ok(Layout::Inside)
}

/// The type this node actually is: what its metadata claims, checked against what the
/// parent's rule assigned. §7.1.
///
/// A disagreement is a [`FindingKind::TypeMismatch`], which the default policy rates
/// `Error` — so the existing refusal is preserved, but a caller can downgrade it.
fn resolve_type(
    path: &EntityPath,
    entity_type: &str,
    metadata: &EntityMeta,
    sink: &mut FindingSink,
) -> anyhow::Result<String> {
    // A malformed source is already reported on its own; falling back here keeps a read
    // that D3 says must survive from failing on the type lookup instead.
    let declared = match metadata.merged() {
        Ok(Some(m)) => m.get_str("type")?,
        Ok(None) | Err(_) => None,
    };

    if entity_type != "Auto" {
        if let Some(found) = declared {
            if found != entity_type {
                sink.report(Finding {
                    path: path.clone(),
                    kind: FindingKind::TypeMismatch {
                        expected: entity_type.to_string(),
                        found,
                    },
                })?;
            }
        }
        return Ok(entity_type.to_string());
    }

    let found = declared.ok_or_else(|| {
        anyhow!(
            "Entity at '{:?}' has Auto type but its metadata is missing the 'type' key",
            path.local_path()
        )
    })?;
    if found == "Auto" {
        bail!(
            "Entity at '{:?}' has metadata 'type' set to 'Auto', which is not allowed",
            path.local_path()
        );
    }
    Ok(found)
}
