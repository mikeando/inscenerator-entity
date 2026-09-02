//! The single authority for turning an [`EntityPath`] into a path on disk.
//!
//! Every path is built from the entity's **stem** — [`stem`] — by appending or descending:
//!
//! ```text
//! book/chapters/ch1              the stem of "chapters/ch1"
//! book/chapters/ch1.md           its content, under ContentLocation::Parallel
//! book/chapters/ch1/content.md   its content, under ContentLocation::Inside
//! book/chapters/ch1.meta.toml    its sidecar, under MetaLocation::ParallelSidecar
//! book/chapters/ch1/meta.toml    its sidecar, under MetaLocation::InsideSidecar
//! book/chapters/ch1/010-intro    the stem of a slash child
//! book/chapters/ch1.review       the stem of a dot child
//! ```
//!
//! Two independent decisions produce those locations. **Layout** — [`Layout`], declared on
//! the entity's own type — chooses between the parallel and inside rows. **Edge** —
//! [`Edge`], declared on the *parent's* rule for this name — chooses between the last two
//! rows. Keeping them orthogonal is what lets a chapter be a readable `ch1.md` and still
//! own a `ch1/` directory of sections (§5).
//!
//! Every suffix is **appended** to the stem, never substituted. Substituting would make
//! the dot-child `a/b.review` resolve to its parent's files (`a/b.md`, `a/b.meta.toml`),
//! which is defect C1 in `docs/storage-layout-v2.md`.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::entity::EntityPath;

/// Where an entity's files live relative to its own stem. Declared on the entity type.
///
/// A type declares one layout, which picks *both* locations — see
/// [`content_location`](Layout::content_location) and
/// [`sidecar_location`](Layout::sidecar_location).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Layout {
    /// `S.md` + `S.meta.toml`
    Parallel,
    /// `S/content.md` + `S/meta.toml`
    Inside,
}

/// Which edge a child attaches on. Declared on the parent's child rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Edge {
    /// `disk(parent)/name`
    Slash,
    /// `disk(parent).name`
    Dot,
}

impl Default for Edge {
    /// §3: dot is the compact case you opt into deliberately.
    fn default() -> Self {
        Edge::Slash
    }
}

/// Observed or intended location of an entity's content. §4.1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContentLocation {
    /// `S.md`
    Parallel,
    /// `S/content.md`
    Inside,
}

/// Observed or intended location of one metadata source. §4.1.
///
/// The ordering is the merge precedence used when a node carries several sources:
/// later locations win. Only reachable when the policy downgrades
/// `MetadataKeyConflict` from its default `Error`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MetaLocation {
    /// Front matter in the content file, wherever layout put that file.
    InHeader,
    /// `S.meta.toml`
    ParallelSidecar,
    /// `S/meta.toml`
    InsideSidecar,
}

impl Layout {
    /// The content location this layout intends — `Parallel` → `<stem>.md`,
    /// `Inside` → `<stem>/content.md`.
    ///
    /// "Intends" rather than "is": a node whose content sits elsewhere still reads, and
    /// the difference is reported as a finding rather than corrected (§4).
    pub fn content_location(self) -> ContentLocation {
        match self {
            Layout::Parallel => ContentLocation::Parallel,
            Layout::Inside => ContentLocation::Inside,
        }
    }

    /// The sidecar location this layout intends — `Parallel` → `<stem>.meta.toml`,
    /// `Inside` → `<stem>/meta.toml`.
    ///
    /// [`MetaLocation::InHeader`] is never *intended* by a layout, because front matter is
    /// orthogonal to layout (§11 D2) — but it is always an acceptable observation, so a
    /// node whose metadata lives in its header is not nonconforming.
    pub fn sidecar_location(self) -> MetaLocation {
        match self {
            Layout::Parallel => MetaLocation::ParallelSidecar,
            Layout::Inside => MetaLocation::InsideSidecar,
        }
    }
}

/// The entity's anchor point on disk — `disk(P)` in the spec — carrying **no extension**.
///
/// Folds the path's entries over `base_path`: a `Slash(name)` entry descends into a
/// subdirectory, a `Dot(name)` entry appends `.name` to the preceding *filename*.
///
/// The stem is never itself a file. It is either a directory or nothing at all — it is
/// the anchor that this entity's content file, its metadata sidecars, and all of its
/// children are derived from, by the other functions in this module.
///
/// ```
/// # use inscenerator_entity::entity::EntityPath;
/// # use inscenerator_entity::placement::stem;
/// # use std::path::{Path, PathBuf};
/// let book = Path::new("book");
///
/// let ch1 = EntityPath::parse("chapters/ch1").unwrap();
/// assert_eq!(stem(book, &ch1), PathBuf::from("book/chapters/ch1"));
///
/// // A dot-child hangs off its parent's filename rather than sitting in a subdirectory.
/// let review = EntityPath::parse("chapters/ch1.review").unwrap();
/// assert_eq!(stem(book, &review), PathBuf::from("book/chapters/ch1.review"));
/// ```
pub fn stem(base_path: &Path, path: &EntityPath) -> PathBuf {
    path.to_pathbuf(base_path)
}

/// The single file this entity's content is read from and written to.
///
/// | `loc` | Path | Shape |
/// | --- | --- | --- |
/// | [`ContentLocation::Parallel`] | `<stem>.md` | a readable spine file, sitting beside the entity's directory |
/// | [`ContentLocation::Inside`] | `<stem>/content.md` | tucked inside the entity's own directory |
///
/// The suffix is **appended** to the stem, never substituted. That is the whole of
/// defect C1: substituting would give the dot-child `chapters/ch1.review` the path
/// `chapters/ch1.md`, which is its *parent's* content file.
///
/// ```
/// # use inscenerator_entity::entity::EntityPath;
/// # use inscenerator_entity::placement::{content_path, ContentLocation};
/// # use std::path::{Path, PathBuf};
/// let book = Path::new("book");
/// let review = EntityPath::parse("chapters/ch1.review").unwrap();
///
/// assert_eq!(
///     content_path(book, &review, ContentLocation::Parallel),
///     PathBuf::from("book/chapters/ch1.review.md"),
/// );
/// assert_eq!(
///     content_path(book, &review, ContentLocation::Inside),
///     PathBuf::from("book/chapters/ch1.review/content.md"),
/// );
/// ```
pub fn content_path(base_path: &Path, path: &EntityPath, loc: ContentLocation) -> PathBuf {
    let s = stem(base_path, path);
    match loc {
        ContentLocation::Parallel => s.with_added_extension("md"),
        ContentLocation::Inside => s.join("content.md"),
    }
}

/// The file **one** metadata source is read from and written to, or `None` when that
/// source has no file of its own.
///
/// A node may carry several sources at once (§4.5), so this answers for one location at
/// a time rather than for the node as a whole.
///
/// | `loc` | Path |
/// | --- | --- |
/// | [`MetaLocation::ParallelSidecar`] | `Some(<stem>.meta.toml)` |
/// | [`MetaLocation::InsideSidecar`] | `Some(<stem>/meta.toml)` |
/// | [`MetaLocation::InHeader`] | `None` — it is front matter inside the content file, wherever [`content_path`] put that |
///
/// The suffix is appended, exactly as in [`content_path`], and for the same reason: a
/// substituted `chapters/ch1.review` would resolve to `chapters/ch1.meta.toml`, so a
/// dot-child would share — and overwrite — its parent's metadata.
///
/// ```
/// # use inscenerator_entity::entity::EntityPath;
/// # use inscenerator_entity::placement::{sidecar_path, MetaLocation};
/// # use std::path::{Path, PathBuf};
/// let book = Path::new("book");
/// let review = EntityPath::parse("chapters/ch1.review").unwrap();
///
/// assert_eq!(
///     sidecar_path(book, &review, MetaLocation::ParallelSidecar),
///     Some(PathBuf::from("book/chapters/ch1.review.meta.toml")),
/// );
/// assert_eq!(
///     sidecar_path(book, &review, MetaLocation::InsideSidecar),
///     Some(PathBuf::from("book/chapters/ch1.review/meta.toml")),
/// );
/// assert_eq!(sidecar_path(book, &review, MetaLocation::InHeader), None);
/// ```
pub fn sidecar_path(base_path: &Path, path: &EntityPath, loc: MetaLocation) -> Option<PathBuf> {
    let s = stem(base_path, path);
    match loc {
        MetaLocation::InHeader => None,
        MetaLocation::ParallelSidecar => Some(s.with_added_extension("meta.toml")),
        MetaLocation::InsideSidecar => Some(s.join("meta.toml")),
    }
}

/// Extends `parent` by one entry to name a child — a **logical** [`EntityPath`], not a
/// path on disk. Feed the result to [`stem`], [`content_path`] or [`sidecar_path`] to
/// reach actual files.
///
/// | `edge` | Child's stem | Shape |
/// | --- | --- | --- |
/// | [`Edge::Slash`] | `<parent stem>/name` | the child sits inside the parent's directory |
/// | [`Edge::Dot`] | `<parent stem>.name` | the child sits beside the parent's file |
///
/// The edge is the *parent's* choice, declared on the child rule that matched this name,
/// and it is independent of where either entity's own files live. Keeping those two
/// decisions orthogonal is what allows a chapter to be a readable `ch1.md` spine file
/// *and* own a `ch1/` directory full of sections (§5).
///
/// ```
/// # use inscenerator_entity::entity::EntityPath;
/// # use inscenerator_entity::placement::{child_path, stem, Edge};
/// # use std::path::{Path, PathBuf};
/// let book = Path::new("book");
/// let ch1 = EntityPath::parse("chapters/ch1").unwrap();
///
/// let section = child_path(&ch1, "010-intro", Edge::Slash);
/// assert_eq!(stem(book, &section), PathBuf::from("book/chapters/ch1/010-intro"));
///
/// let notes = child_path(&ch1, "notes", Edge::Dot);
/// assert_eq!(stem(book, &notes), PathBuf::from("book/chapters/ch1.notes"));
/// ```
pub fn child_path(parent: &EntityPath, name: &str, edge: Edge) -> EntityPath {
    match edge {
        Edge::Slash => parent.extend_slash(name),
        Edge::Dot => parent.extend_dot(name),
    }
}

/// Filenames the slash pass must skip — they belong to the parent, not to a child. §3.1.
pub const RESERVED_SLASH_NAMES: [&str; 3] = ["content.md", "meta.toml", "schema.toml"];

/// Dot suffixes the dot pass must skip — they are the entity's own files. §3.1.
pub const RESERVED_DOT_SUFFIXES: [&str; 2] = ["md", "meta.toml"];

#[cfg(test)]
mod tests {
    use super::*;

    fn p(spec: &str) -> EntityPath {
        EntityPath::parse(spec).unwrap()
    }

    fn base() -> &'static Path {
        Path::new("base")
    }

    /// §1: `disk(P)` folds the entries over the base path, and is always a bare stem.
    #[test]
    fn stem_folds_entries_and_never_carries_an_extension() {
        for (spec, expected) in [
            ("", "base"),
            ("chapters/ch1", "base/chapters/ch1"),
            ("chapters/ch1.notes", "base/chapters/ch1.notes"),
            ("chapters/ch1.notes.draft", "base/chapters/ch1.notes.draft"),
        ] {
            assert_eq!(stem(base(), &p(spec)), PathBuf::from(expected), "stem of {:?}", spec);
        }
    }

    /// §2 / C1: suffixes are **appended** to the stem. Under substitution the dot-child
    /// `ch1.review` would resolve to its parent `ch1`'s files, so one sidecar would
    /// serve both entities — and writing the child would clobber the parent.
    #[test]
    fn suffixes_are_appended_never_substituted() {
        let child = p("chapters/ch1.review");
        assert_eq!(
            content_path(base(), &child, ContentLocation::Parallel),
            PathBuf::from("base/chapters/ch1.review.md")
        );
        assert_eq!(
            sidecar_path(base(), &child, MetaLocation::ParallelSidecar),
            Some(PathBuf::from("base/chapters/ch1.review.meta.toml"))
        );

        // The parent's files — the ones substitution would have collided with.
        let parent = p("chapters/ch1");
        assert_eq!(
            content_path(base(), &parent, ContentLocation::Parallel),
            PathBuf::from("base/chapters/ch1.md")
        );
        assert_eq!(
            sidecar_path(base(), &parent, MetaLocation::ParallelSidecar),
            Some(PathBuf::from("base/chapters/ch1.meta.toml"))
        );
    }

    /// §2: `inside` puts both files in the stem's own directory — dot-children included.
    #[test]
    fn inside_layout_puts_both_files_in_the_stem_directory() {
        let child = p("chapters/ch1.review");
        assert_eq!(
            content_path(base(), &child, ContentLocation::Inside),
            PathBuf::from("base/chapters/ch1.review/content.md")
        );
        assert_eq!(
            sidecar_path(base(), &child, MetaLocation::InsideSidecar),
            Some(PathBuf::from("base/chapters/ch1.review/meta.toml"))
        );
    }

    /// §4.1: in-header metadata has no file of its own.
    #[test]
    fn in_header_metadata_has_no_sidecar_path() {
        assert_eq!(sidecar_path(base(), &p("chapters/ch1"), MetaLocation::InHeader), None);
    }

    /// §2: a type declares ONE layout, which picks both locations. `S.md` + `S/meta.toml`
    /// is not something a schema can declare — though §4.1 can still observe it.
    #[test]
    fn a_layout_picks_both_content_and_sidecar_locations() {
        assert_eq!(Layout::Parallel.content_location(), ContentLocation::Parallel);
        assert_eq!(Layout::Parallel.sidecar_location(), MetaLocation::ParallelSidecar);
        assert_eq!(Layout::Inside.content_location(), ContentLocation::Inside);
        assert_eq!(Layout::Inside.sidecar_location(), MetaLocation::InsideSidecar);
    }

    /// §3: the edge decides the child's stem, and defaults to slash.
    #[test]
    fn edge_decides_the_child_stem_and_defaults_to_slash() {
        let parent = p("chapters/ch1");
        assert_eq!(
            stem(base(), &child_path(&parent, "notes", Edge::Slash)),
            PathBuf::from("base/chapters/ch1/notes")
        );
        assert_eq!(
            stem(base(), &child_path(&parent, "notes", Edge::Dot)),
            PathBuf::from("base/chapters/ch1.notes")
        );
        assert_eq!(Edge::default(), Edge::Slash);
    }

    /// §2 / §3: the spellings a schema author writes in `schema.toml`.
    #[test]
    fn layout_and_edge_use_lowercase_toml_spellings() {
        #[derive(Deserialize)]
        struct Decl {
            layout: Layout,
            edge: Edge,
        }
        let decl: Decl = toml::from_str("layout = \"parallel\"\nedge = \"dot\"").unwrap();
        assert_eq!(decl.layout, Layout::Parallel);
        assert_eq!(decl.edge, Edge::Dot);

        let decl: Decl = toml::from_str("layout = \"inside\"\nedge = \"slash\"").unwrap();
        assert_eq!(decl.layout, Layout::Inside);
        assert_eq!(decl.edge, Edge::Slash);
    }
}
