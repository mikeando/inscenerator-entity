# Storage Layout v2 — Core Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **On completion of planning, copy this file to `docs/superpowers/plans/2026-09-02-storage-layout-v2-core.md`** (plan mode could not create it there).

**Goal:** Make the schema declare where a child attaches (`edge`) and where its files live (`layout`), so a caller can create a child without knowing the on-disk convention — and make every read tolerant of trees that don't match, reporting the difference instead of failing.

**Architecture:** All path arithmetic moves into one new module (`placement.rs`) that is the single authority for stems, content paths and sidecar paths — appending suffixes, never substituting. A second new module (`findings.rs`) carries the diagnostic vocabulary and a per-kind severity policy. Child discovery, rule matching and placement resolution each get exactly one implementation, shared by the eager reader (`EntityLoader`) and the lazy handle (`LiveEntity`). Reads probe disk and tolerate what they find; writes route to whatever a node has already established, falling back to schema intent only for genuinely new things.

**Tech Stack:** Rust (edition 2018), `anyhow`, `serde`, `toml 0.9` (`preserve_order`), `regex 1`, `yaml-rust 0.4.5`, `inscenerator-xfs 0.1.4` (`Xfs` trait + `MockFS`). All tests are inline `#[cfg(test)] mod tests` using `MockFS`. Baseline: 75 tests passing at commit `1cb17c4`.

**Spec:** `docs/storage-layout-v2.md`. `docs/storage-layout.md` is the as-built description of the code being replaced.

> **Historical record.** This plan is complete. The spec it cites no longer exists as a separate
> file: `docs/storage-layout.md` now describes the layout as built, keeping the same section
> numbers, so a `§n` reference below still resolves there. The C-numbered defects were all fixed
> and the D-numbered decisions are all stated in that document.

---

## Context

Three placement facts — content location, metadata location, and which edge a child hangs off — are currently decided independently, and none is written down anywhere the library can consult. The reader infers all three from what happens to be on disk; the writer guesses two from a heuristic (`ChildContentLayout::Inferred`); the schema knows none of them. The consequence is that **a caller cannot create a child without already knowing the on-disk convention**, which is why `inscenerator-booker-agents4` bypasses `LiveEntity` and builds paths by string concatenation in Lua (`templates/nonfiction-structured/.config/actions/generate-section-outline.lua:32`).

The spec's fix is two declarations: the parent's rule says which **edge** a child attaches on; the child's type says where its **content and metadata** live. They are orthogonal, which is what keeps §5's shape — a readable `chapter.md` spine file with a `chapter/` directory of sections beside it — expressible at all.

Along the way this fixes a set of defects the spec enumerates as C1–C11, the worst of which is C1: a dot-child's parallel sidecar path is computed by *substitution* in three places (`entity.rs:516`, `entity.rs:693`, `live_entity.rs:912`) and by *appending* in two (`live_entity.rs:360`, `:1000`). Under substitution the dot-child `a/b.review` and its parent `a/b` resolve to the same `a/b.meta.toml`, so writing a dot-child **overwrites its parent's metadata**, and `EntityLoader` already cannot read back what `LiveEntity` writes.

**Backwards compatibility is explicitly not a goal.** Existing API shapes are deleted outright rather than deprecated.

---

## Global Constraints

- Rust edition **2018**. `Path::with_added_extension` and `Path::file_prefix` are already used throughout and remain available on the toolchain in use — do not replace them.
- **Suffixes are appended to the stem, never substituted.** `.with_extension("meta.toml")` must not appear anywhere in `src/` when this plan is done. Grep for it as a completion check.
- `disk(P)` never carries an extension — it is a **stem**.
- **The root is always `inside`**, must be a directory, and has no dot children. A root type declaring `layout = "parallel"`, or a root rule declaring `edge = "dot"`, is a schema error.
- **An entity name may not contain a `.`** — `my.file.md` is entity `my` with dot-child `file`.
- **Rule matching has exactly one implementation**, called from every site that consults rules.
- **Reading is tolerant.** A node that does not match its type's intent still loads; the difference is recorded as a finding. Findings of severity `Error` (per the configured policy) abort the call that produced them.
- Reserved filenames skipped by the slash pass: `content.md`, `meta.toml`, `schema.toml`. Skipped by the dot pass: suffixes `md` and `meta.toml`.
- Every task ends green: `cargo test` passes, `cargo build --examples` passes.

## Decisions ruled during planning (these override the spec's "leaning" text)

| # | Ruling |
| --- | --- |
| **D1** | `required` / `multiple` are enforced as **findings** under the D6 policy, default `Warn`. |
| **D2** | Header and sidecar metadata **merge**, routed per key. Same key in two sources with different values is a **`MetadataKeyConflict` finding, default `Error`**. |
| **D3** | Malformed metadata is **never silently dropped and never fatal by default**. The source is retained in an error state carrying its raw text and the parse error, so callers can inspect both. When sources are merged, each source keeps its own state. |
| **D4** | `Entity` (eager) gains a `findings: Vec<Finding>` field populated at load. `LiveEntity` (lazy) gains **`issues()`**, which probes disk at call time and reports that node's findings. |
| **D5** | `move_to` **relocates only** — it preserves each node's layout at the new stem. Resulting nonconformance is reported, not repaired. |
| **D6** | Severity is **configurable per finding kind** via a `FindingPolicy`. `EntityLoader` walks the whole tree so it can fail up-front; `LiveEntity` is lazy so an `Error` finding aborts only the accessor that produced it. |
| **Scope** | §9.2–9.4 (`NormaliseSpec`, `plan_normalise`, `normalise`, `MovePlan`, atomicity) and D7/D8 are **out of scope** — a follow-up plan. §8.2 (the Lua `add_child{}` table call) lives in `inscenerator-booker-agents4`, not this crate, and is also out of scope. |

---

## File Structure

| File | Responsibility |
| --- | --- |
| `src/placement.rs` | **New.** `Layout`, `Edge`, `ContentLocation`, `MetaLocation` and every path-building function. The single authority for turning an `EntityPath` into a file path. |
| `src/findings.rs` | **New.** `Finding`, `FindingKind`, `Severity`, `FindingPolicy`. No I/O. |
| `src/discovery.rs` | **New.** Child discovery (both passes), cross-pass dedup, intent tie-breaking, per-rule edge observation. Shared by both readers. |
| `src/schema.rs` | Modify. `edge` on rules, `layout` on types, regex `ignore`, compiled regexes, the one `match_child`. |
| `src/entity.rs` | Modify. `EntityMeta` becomes a source list; `EntityLoader` and `EntityWriter` rebuilt on the modules above. `utils` keeps front-matter parsing. |
| `src/live_entity.rs` | Modify. `LiveEntity` read/write/`issues()`, `ChildBuilder` rebuilt, `move_to` fixed. |
| `src/lib.rs` | Modify. Re-export the new modules. |
| `examples/create_child.rs` | Modify. `create_child` takes a name; `ChildContentLayout` builders are gone. |
| `README.md` | Modify. C11 — root metadata line. |
| `docs/storage-layout.md` | Replace with a v2 as-built description at the end. |

`entity.rs` and `live_entity.rs` are both ~75KB and already unwieldy; this plan moves substantial logic out of them into the three new modules rather than growing them further.

---

## Task 1: `placement.rs` — the path authority

**Files:**
- Create: `src/placement.rs`
- Modify: `src/lib.rs`

**Interfaces:**
- Produces: `Layout`, `Edge`, `ContentLocation`, `MetaLocation`, `stem()`, `content_path()`, `sidecar_path()`, `child_path()`. Every later task uses these instead of building paths inline.

- [ ] **Step 1: Write the failing tests**

Create `src/placement.rs` with the test module only, then add the implementation in step 3.

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityPath;
    use std::path::{Path, PathBuf};

    fn ch1() -> EntityPath {
        EntityPath::empty().extend_slash("chapters").extend_slash("ch1")
    }
    fn ch1_review() -> EntityPath {
        ch1().extend_dot("review")
    }

    #[test]
    fn stem_never_has_an_extension() {
        assert_eq!(stem(Path::new("base"), &ch1()), PathBuf::from("base/chapters/ch1"));
        assert_eq!(
            stem(Path::new("base"), &ch1_review()),
            PathBuf::from("base/chapters/ch1.review")
        );
    }

    #[test]
    fn content_paths_append_never_substitute() {
        let b = Path::new("base");
        assert_eq!(
            content_path(b, &ch1_review(), ContentLocation::Parallel),
            PathBuf::from("base/chapters/ch1.review.md")
        );
        assert_eq!(
            content_path(b, &ch1_review(), ContentLocation::Inside),
            PathBuf::from("base/chapters/ch1.review/content.md")
        );
    }

    /// C1: the dot-child's sidecar must NOT collapse onto its parent's sidecar.
    #[test]
    fn dot_child_sidecar_does_not_collide_with_parent() {
        let b = Path::new("base");
        let parent = sidecar_path(b, &ch1(), MetaLocation::ParallelSidecar).unwrap();
        let child = sidecar_path(b, &ch1_review(), MetaLocation::ParallelSidecar).unwrap();
        assert_eq!(parent, PathBuf::from("base/chapters/ch1.meta.toml"));
        assert_eq!(child, PathBuf::from("base/chapters/ch1.review.meta.toml"));
        assert_ne!(parent, child);
    }

    #[test]
    fn inside_sidecar_is_inside_the_stem_directory() {
        assert_eq!(
            sidecar_path(Path::new("base"), &ch1_review(), MetaLocation::InsideSidecar).unwrap(),
            PathBuf::from("base/chapters/ch1.review/meta.toml")
        );
    }

    #[test]
    fn in_header_has_no_sidecar_path() {
        assert!(sidecar_path(Path::new("base"), &ch1(), MetaLocation::InHeader).is_none());
    }

    #[test]
    fn child_path_follows_the_edge() {
        assert_eq!(
            child_path(&ch1(), "notes", Edge::Slash),
            ch1().extend_slash("notes")
        );
        assert_eq!(
            child_path(&ch1(), "notes", Edge::Dot),
            ch1().extend_dot("notes")
        );
    }

    #[test]
    fn layout_maps_to_both_locations() {
        assert_eq!(Layout::Parallel.content_location(), ContentLocation::Parallel);
        assert_eq!(Layout::Parallel.sidecar_location(), MetaLocation::ParallelSidecar);
        assert_eq!(Layout::Inside.content_location(), ContentLocation::Inside);
        assert_eq!(Layout::Inside.sidecar_location(), MetaLocation::InsideSidecar);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test placement`
Expected: FAIL — `placement` module not declared / items not found.

- [ ] **Step 3: Write the implementation**

Add above the test module in `src/placement.rs`:

```rust
//! The single authority for turning an `EntityPath` into a path on disk.
//!
//! Every suffix is **appended** to the stem, never substituted. Substituting would
//! make the dot-child `a/b.review` resolve to its parent's files (`a/b.meta.toml`),
//! which is defect C1 in `docs/storage-layout-v2.md`.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::entity::EntityPath;

/// Where an entity's files live, relative to its own stem. Declared on the entity type.
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
    pub fn content_location(self) -> ContentLocation {
        match self {
            Layout::Parallel => ContentLocation::Parallel,
            Layout::Inside => ContentLocation::Inside,
        }
    }

    /// The sidecar location this layout intends. `InHeader` is never *intended* by a
    /// layout — it is orthogonal to layout (§11 D2) and always an acceptable observation.
    pub fn sidecar_location(self) -> MetaLocation {
        match self {
            Layout::Parallel => MetaLocation::ParallelSidecar,
            Layout::Inside => MetaLocation::InsideSidecar,
        }
    }
}

/// `disk(P)` — folds the entries over `base_path`. Never carries an extension.
pub fn stem(base_path: &Path, path: &EntityPath) -> PathBuf {
    path.to_pathbuf(base_path)
}

/// The file holding this entity's content at the given location.
pub fn content_path(base_path: &Path, path: &EntityPath, loc: ContentLocation) -> PathBuf {
    let s = stem(base_path, path);
    match loc {
        ContentLocation::Parallel => s.with_added_extension("md"),
        ContentLocation::Inside => s.join("content.md"),
    }
}

/// The sidecar file at the given location, or `None` for `InHeader` (which has no file
/// of its own — it lives in whatever file `content_path` names).
pub fn sidecar_path(base_path: &Path, path: &EntityPath, loc: MetaLocation) -> Option<PathBuf> {
    let s = stem(base_path, path);
    match loc {
        MetaLocation::InHeader => None,
        MetaLocation::ParallelSidecar => Some(s.with_added_extension("meta.toml")),
        MetaLocation::InsideSidecar => Some(s.join("meta.toml")),
    }
}

/// The logical path of a child of `parent` named `name` on the given edge.
pub fn child_path(parent: &EntityPath, name: &str, edge: Edge) -> EntityPath {
    match edge {
        Edge::Slash => parent.extend_slash(name),
        Edge::Dot => parent.extend_dot(name),
    }
}

/// Reserved filenames the slash pass must skip — they belong to the parent, not to a child.
pub const RESERVED_SLASH_NAMES: [&str; 3] = ["content.md", "meta.toml", "schema.toml"];

/// Reserved dot suffixes the dot pass must skip — they are the entity's own files.
pub const RESERVED_DOT_SUFFIXES: [&str; 2] = ["md", "meta.toml"];
```

In `src/lib.rs` add `pub mod placement;` above `pub mod schema;`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test placement`
Expected: PASS, 7 tests.

- [ ] **Step 5: Commit**

```bash
git add src/placement.rs src/lib.rs
git commit -m "feat: add placement module as the single path authority

Suffixes are appended to the stem, never substituted, which is the
root fix for C1 (dot-child sidecar colliding with its parent's)."
```

---

## Task 2: Schema — `edge`, `layout`, regex `ignore`, one rule matcher

**Files:**
- Modify: `src/schema.rs` (whole file — `ChildEntityRules`, `EntityTypeDescription`, `Schema`, `child_type`)

**Interfaces:**
- Consumes: `placement::{Edge, Layout}` (Task 1).
- Produces: `ChildEntityRules.edge`, `EntityTypeDescription.layout`, `CompiledRule`, `CompiledType`, `ChildMatch`, `Schema::compiled(&str) -> Result<&CompiledType>`, `CompiledType::match_child(&str) -> ChildMatch`, `Schema::add_entity_type(...) -> anyhow::Result<()>`.

This addresses **C4** (schema cannot distinguish dot from slash), **C5** (two rule-matching implementations), **C6** (`ignore` exact-match while `children` is regex).

- [ ] **Step 1: Write the failing tests**

Append to `src/schema.rs`'s `mod tests`:

```rust
    fn schema_with(toml_src: &str) -> anyhow::Result<Schema> {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "project", "schema.toml", toml_src);
        Schema::load_from_file(&fs, &Path::new("project/schema.toml"))
    }

    const EDGE_SCHEMA: &str = r#"
[Chapter]
allow_additional = false
layout = "parallel"
ignore = ['^\.gitkeep$', '^README\.md$']
[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"
[[Chapter.children]]
name_regex = '^notes$'
node_type = "Note"
edge = "dot"

[Section]
allow_additional = false
children = []

[Note]
allow_additional = false
children = []
"#;

    #[test]
    fn rules_carry_an_edge_and_types_carry_a_layout() {
        let s = schema_with(EDGE_SCHEMA).unwrap();
        let c = &s.entity_types["Chapter"];
        assert_eq!(c.layout, Some(Layout::Parallel));
        assert_eq!(c.children[0].edge, Edge::Slash);
        assert_eq!(c.children[1].edge, Edge::Dot);
        // Section omits both -> layout inherits at load time, edge defaults to slash.
        assert_eq!(s.entity_types["Section"].layout, None);
    }

    #[test]
    fn edge_defaults_to_slash_when_omitted() {
        let s = schema_with(
            r#"
[T]
allow_additional = false
[[T.children]]
name_regex = "x"
node_type = "T"
"#,
        )
        .unwrap();
        assert_eq!(s.entity_types["T"].children[0].edge, Edge::Slash);
        assert!(!s.entity_types["T"].children[0].required);
        assert!(!s.entity_types["T"].children[0].multiple);
    }

    #[test]
    fn match_child_dispatches_ignore_rule_additional_and_unexpected() {
        let s = schema_with(EDGE_SCHEMA).unwrap();
        let ct = s.compiled("Chapter").unwrap();
        assert!(matches!(ct.match_child(".gitkeep"), ChildMatch::Ignored));
        assert!(matches!(ct.match_child("README.md"), ChildMatch::Ignored));
        match ct.match_child("010-intro") {
            ChildMatch::Matched(r) => {
                assert_eq!(r.rule.node_type, "Section");
                assert_eq!(r.rule.edge, Edge::Slash);
                assert_eq!(r.index, 0);
            }
            other => panic!("expected Matched, got {:?}", other),
        }
        match ct.match_child("notes") {
            ChildMatch::Matched(r) => assert_eq!(r.rule.edge, Edge::Dot),
            other => panic!("expected Matched, got {:?}", other),
        }
        // allow_additional = false, no rule matches
        assert!(matches!(ct.match_child("stray"), ChildMatch::Unexpected));
    }

    #[test]
    fn additional_is_returned_when_allow_additional_is_set() {
        let s = schema_with(
            r#"
[T]
allow_additional = true
children = []
"#,
        )
        .unwrap();
        assert!(matches!(s.compiled("T").unwrap().match_child("anything"), ChildMatch::Additional));
    }

    /// C6: ignore is a regex list matching `children`, not exact strings.
    #[test]
    fn ignore_entries_are_regexes() {
        let s = schema_with(
            r#"
[T]
allow_additional = false
ignore = ['^\.']
children = []
"#,
        )
        .unwrap();
        let ct = s.compiled("T").unwrap();
        assert!(matches!(ct.match_child(".hidden"), ChildMatch::Ignored));
        assert!(matches!(ct.match_child("visible"), ChildMatch::Unexpected));
    }

    /// C5: an invalid regex is rejected once, when the schema is built — so load and
    /// lookup cannot disagree, because neither gets a schema to disagree over.
    #[test]
    fn invalid_regex_fails_at_schema_build() {
        let err = schema_with(
            r#"
[T]
allow_additional = false
[[T.children]]
name_regex = "["
node_type = "T"
"#,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("Invalid regex"), "got: {}", err);
        assert!(err.contains('['), "got: {}", err);
    }

    #[test]
    fn invalid_ignore_regex_fails_at_schema_build() {
        assert!(schema_with(
            r#"
[T]
allow_additional = false
ignore = ["("]
children = []
"#,
        )
        .is_err());
    }
```

Add `use crate::placement::{Edge, Layout};` to the test module's imports (or rely on `use super::*`).

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib schema`
Expected: FAIL — `layout`/`edge` fields and `compiled`/`ChildMatch` do not exist.

- [ ] **Step 3: Write the implementation**

Replace the type definitions and `impl EntityTypeDescription` / `impl Schema` in `src/schema.rs`:

```rust
use crate::placement::{Edge, Layout};

/// Rules for discovering and validating child entities.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    /// Regex matched against the child's name.
    pub name_regex: String,
    /// The expected entity type for matching children.
    pub node_type: String,
    /// Whether at least one child matching this rule must exist. Reported, not enforced (D1).
    #[serde(default)]
    pub required: bool,
    /// Whether more than one child may match this rule. Reported, not enforced (D1).
    #[serde(default)]
    pub multiple: bool,
    /// Which edge children matching this rule attach on. §3. Defaults to `slash`.
    #[serde(default)]
    pub edge: Edge,
}

/// Description of an entity type, including its allowed children.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EntityTypeDescription {
    pub name: String,
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
    /// Regexes matched against child names; matching names produce no child at all. §7.4.
    #[serde(default)]
    pub ignore: Vec<String>,
    /// Where this type's content and sidecar live. `None` inherits from the parent
    /// *instance* this type was loaded under (§2.1). The root is always `Inside`.
    #[serde(default)]
    pub layout: Option<Layout>,
}

/// A child rule with its regex compiled and its position in the rule list recorded.
/// The index identifies the rule in findings (§4.2 observes edge *per rule*).
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub rule: ChildEntityRules,
    pub index: usize,
    re: regex::Regex,
}

impl CompiledRule {
    pub fn is_match(&self, name: &str) -> bool {
        self.re.is_match(name)
    }
}

/// An entity type with all its regexes compiled. Built once, at schema construction.
#[derive(Debug, Clone)]
pub struct CompiledType {
    pub desc: EntityTypeDescription,
    pub rules: Vec<CompiledRule>,
    ignore: Vec<regex::Regex>,
}

/// The outcome of matching a child name against a type's rules. §7.1 — this is the
/// *only* rule-matching implementation; every consulting site calls `match_child`.
#[derive(Debug)]
pub enum ChildMatch<'a> {
    /// Matched an `ignore` regex — produces no child, and is not an error.
    Ignored,
    /// Matched a child rule, which supplies the type and the intended edge.
    Matched(&'a CompiledRule),
    /// No rule matched, but `allow_additional = true`. No declared edge, no declared type. §7.2.
    Additional,
    /// No rule matched and `allow_additional = false`.
    Unexpected,
}

impl CompiledType {
    pub fn match_child(&self, name: &str) -> ChildMatch<'_> {
        if self.ignore.iter().any(|re| re.is_match(name)) {
            return ChildMatch::Ignored;
        }
        for r in &self.rules {
            if r.is_match(name) {
                return ChildMatch::Matched(r);
            }
        }
        if self.desc.allow_additional {
            ChildMatch::Additional
        } else {
            ChildMatch::Unexpected
        }
    }
}

fn compile(desc: &EntityTypeDescription) -> anyhow::Result<CompiledType> {
    let mut rules = Vec::with_capacity(desc.children.len());
    for (index, rule) in desc.children.iter().enumerate() {
        let re = regex::Regex::new(&rule.name_regex)
            .map_err(|e| anyhow!("Invalid regex in schema for {}: {}", rule.name_regex, e))?;
        rules.push(CompiledRule { rule: rule.clone(), index, re });
    }
    let mut ignore = Vec::with_capacity(desc.ignore.len());
    for pat in &desc.ignore {
        ignore.push(
            regex::Regex::new(pat)
                .map_err(|e| anyhow!("Invalid regex in schema ignore for {}: {}", pat, e))?,
        );
    }
    Ok(CompiledType { desc: desc.clone(), rules, ignore })
}
```

`Schema` gains a parallel compiled map. Keep `entity_types` as the serde-facing source of truth so existing reads of `schema.entity_types["X"]` in tests keep working:

```rust
#[derive(Default, Clone, Debug)]
pub struct Schema {
    pub entity_types: HashMap<String, EntityTypeDescription>,
    compiled: HashMap<String, CompiledType>,
}

impl Schema {
    pub fn new() -> Self { Self::default() }

    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// The compiled form, which is what every rule-matching site should use.
    pub fn compiled(&self, entity_type: &str) -> anyhow::Result<&CompiledType> {
        self.compiled
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// Adds a type, compiling its regexes.
    ///
    /// # Errors
    /// Returns an error if any `name_regex` or `ignore` pattern is not a valid regex.
    pub fn add_entity_type(&mut self, description: EntityTypeDescription) -> anyhow::Result<()> {
        let c = compile(&description)?;
        self.compiled.insert(description.name.clone(), c);
        self.entity_types.insert(description.name.clone(), description);
        Ok(())
    }
}
```

`load_from_file` builds the map then calls `add_entity_type` for each, propagating errors. Add `layout` to `RawEntityTypeDescription` with `#[serde(default)]`, and give its `children` field `#[serde(default)]` too so `children = []` may be omitted.

Delete `EntityTypeDescription::child_type` entirely — `match_child` replaces it. Fix the callers in `entity.rs:604-605` and `live_entity.rs:695`, `:1011`, `:1020` provisionally (they are rewritten properly in Tasks 6 and 8); for now have them call `match_child` so the crate compiles.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --lib schema && cargo test`
Expected: schema tests PASS. Existing tests that call `add_entity_type` now need `.unwrap()` — fix each call site in `entity.rs`'s `dummy_loader` (`entity.rs:836-860`), `live_entity.rs`'s `setup_schema` (`live_entity.rs:1056-1070`), and `examples/create_child.rs`. Full suite green.

- [ ] **Step 5: Commit**

```bash
git add src/schema.rs src/entity.rs src/live_entity.rs examples/create_child.rs
git commit -m "feat: schema declares edge and layout; one compiled rule matcher

Fixes C4 (no edge in schema), C5 (two matching implementations),
C6 (ignore was exact-match while children was regex)."
```

---

## Task 3: `EntityMeta` becomes a list of sources

**Files:**
- Modify: `src/entity.rs` (the `EntityMeta` enum at `:754-799`, `utils::try_load_file_as_metadata` at `:272`, and every construction/match site)

**Interfaces:**
- Consumes: `placement::MetaLocation` (Task 1).
- Produces: `MetaState`, `MetaSource`, `EntityMeta { sources }`, `EntityMeta::merged()`, `::location_of()`, `::malformed()`, `::conflicts()`, `::get_str()`, `::single()`, `::of()`.

This implements **D2** (merge) and the read half of **D3** (malformed sources are retained, not dropped and not fatal). It deletes the `"Multiple metadata sources found"` bail at `entity.rs:536` and `live_entity.rs:651`.

- [ ] **Step 1: Write the failing tests**

Add a new `mod meta_tests` inside `entity.rs`'s existing `#[cfg(test)] mod tests`:

```rust
    mod meta_tests {
        use super::super::*;
        use crate::placement::MetaLocation;

        fn parsed(loc: MetaLocation, src: &str) -> MetaSource {
            MetaSource {
                location: loc,
                state: MetaState::Parsed(Metadata { value: toml::from_str(src).unwrap() }),
                header: None,
            }
        }

        #[test]
        fn no_sources_is_none() {
            let m = EntityMeta::default();
            assert!(m.is_none());
            assert!(m.merged().unwrap().is_none());
        }

        #[test]
        fn one_source_behaves_like_today() {
            let m = EntityMeta::of(vec![parsed(MetaLocation::ParallelSidecar, "type = \"T\"")]);
            assert_eq!(m.get_str("type").unwrap().as_deref(), Some("T"));
            assert_eq!(m.location_of("type"), Some(MetaLocation::ParallelSidecar));
        }

        /// D2: non-conflicting keys from two sources merge.
        #[test]
        fn disjoint_keys_merge_across_sources() {
            let m = EntityMeta::of(vec![
                parsed(MetaLocation::InHeader, "type = \"T\""),
                parsed(MetaLocation::ParallelSidecar, "word_count = 12"),
            ]);
            let merged = m.merged().unwrap().unwrap();
            assert_eq!(merged.get_str("type").unwrap().as_deref(), Some("T"));
            assert_eq!(merged.value.get("word_count").unwrap().as_integer(), Some(12));
            // §4.5: per-key routing knows where each key lives.
            assert_eq!(m.location_of("type"), Some(MetaLocation::InHeader));
            assert_eq!(m.location_of("word_count"), Some(MetaLocation::ParallelSidecar));
            assert_eq!(m.location_of("nothing"), None);
            assert!(m.conflicts().is_empty());
        }

        #[test]
        fn same_key_same_value_is_not_a_conflict() {
            let m = EntityMeta::of(vec![
                parsed(MetaLocation::InHeader, "type = \"T\""),
                parsed(MetaLocation::ParallelSidecar, "type = \"T\""),
            ]);
            assert!(m.conflicts().is_empty());
            assert!(m.merged().is_ok());
        }

        /// D2 ruling: same key, different values, is a conflict.
        #[test]
        fn same_key_different_value_is_a_conflict() {
            let m = EntityMeta::of(vec![
                parsed(MetaLocation::InHeader, "type = \"A\""),
                parsed(MetaLocation::ParallelSidecar, "type = \"B\""),
            ]);
            let c = m.conflicts();
            assert_eq!(c.len(), 1);
            assert_eq!(c[0].key, "type");
            assert_eq!(
                c[0].locations,
                vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar]
            );
            // Documented precedence, only reachable when the policy downgrades the finding:
            // later location wins, ordered InHeader < ParallelSidecar < InsideSidecar.
            let merged = m.merged().unwrap().unwrap();
            assert_eq!(merged.get_str("type").unwrap().as_deref(), Some("B"));
        }

        /// D3: a malformed source keeps its raw text and its error, and does not
        /// destroy the other source's keys.
        #[test]
        fn malformed_source_is_retained_alongside_a_good_one() {
            let m = EntityMeta::of(vec![
                MetaSource {
                    location: MetaLocation::ParallelSidecar,
                    state: MetaState::Malformed {
                        raw: "this is not = = toml".to_string(),
                        error: "expected value".to_string(),
                    },
                    header: None,
                },
                parsed(MetaLocation::InHeader, "type = \"T\""),
            ]);
            assert!(!m.is_none());
            let bad = m.malformed();
            assert_eq!(bad.len(), 1);
            assert_eq!(bad[0].location, MetaLocation::ParallelSidecar);
            assert!(bad[0].raw().unwrap().contains("not = = toml"));
            assert!(bad[0].error().unwrap().contains("expected value"));
            // The good source still merges.
            assert_eq!(m.get_str("type").unwrap().as_deref(), Some("T"));
        }
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib meta_tests`
Expected: FAIL — `MetaSource`, `MetaState`, `EntityMeta::of` do not exist.

- [ ] **Step 3: Write the implementation**

Replace `EntityMeta` in `src/entity.rs`:

```rust
/// The parse state of one metadata source. A source that failed to parse keeps its
/// raw text and the parse error so a caller can inspect and repair it (D3).
#[derive(Debug, PartialEq, Clone)]
pub enum MetaState {
    Parsed(Metadata),
    Malformed { raw: String, error: String },
}

/// One metadata source as observed on disk.
#[derive(Debug, PartialEq, Clone)]
pub struct MetaSource {
    pub location: MetaLocation,
    pub state: MetaState,
    /// Front-matter shape, present only for `MetaLocation::InHeader`.
    pub header: Option<HeaderInfo>,
}

/// How in-header metadata was written, so it can be round-tripped byte-compatibly.
#[derive(Debug, PartialEq, Clone)]
pub struct HeaderInfo {
    pub separator: Option<String>,
    pub header_type: HeaderType,
}

impl MetaSource {
    pub fn metadata(&self) -> Option<&Metadata> {
        match &self.state {
            MetaState::Parsed(m) => Some(m),
            MetaState::Malformed { .. } => None,
        }
    }
    pub fn raw(&self) -> Option<&str> {
        match &self.state {
            MetaState::Malformed { raw, .. } => Some(raw),
            MetaState::Parsed(_) => None,
        }
    }
    pub fn error(&self) -> Option<&str> {
        match &self.state {
            MetaState::Malformed { error, .. } => Some(error),
            MetaState::Parsed(_) => None,
        }
    }
    pub fn is_malformed(&self) -> bool {
        matches!(self.state, MetaState::Malformed { .. })
    }
}

/// One key present in two or more sources with differing values. §4.5's unresolvable case.
#[derive(Debug, PartialEq, Clone)]
pub struct MetaConflict {
    pub key: String,
    pub locations: Vec<MetaLocation>,
}

/// An entity's metadata, as the set of sources observed on disk.
///
/// Zero sources means no metadata. One source is the ordinary case. Several sources
/// merge per key (§4.5) — which is *observable but not intendable*, and is reported as
/// ambiguity by §9.1.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct EntityMeta {
    sources: Vec<MetaSource>,
}

impl EntityMeta {
    /// Builds from sources, normalising their order to `MetaLocation`'s ordering
    /// (`InHeader` < `ParallelSidecar` < `InsideSidecar`) so merge precedence is
    /// deterministic regardless of probe order.
    pub fn of(mut sources: Vec<MetaSource>) -> Self {
        sources.sort_by_key(|s| s.location);
        EntityMeta { sources }
    }

    pub fn sources(&self) -> &[MetaSource] { &self.sources }

    pub fn is_none(&self) -> bool { self.sources.is_empty() }

    /// The single source, if there is exactly one. Convenience for the common case.
    pub fn single(&self) -> Option<&MetaSource> {
        if self.sources.len() == 1 { self.sources.first() } else { None }
    }

    pub fn locations(&self) -> Vec<MetaLocation> {
        self.sources.iter().map(|s| s.location).collect()
    }

    pub fn malformed(&self) -> Vec<&MetaSource> {
        self.sources.iter().filter(|s| s.is_malformed()).collect()
    }

    /// Which source holds `key`, for §4.5's per-key write routing. Where a key appears
    /// in several sources, the winning source under merge precedence is returned.
    pub fn location_of(&self, key: &str) -> Option<MetaLocation> {
        self.sources
            .iter()
            .rev()
            .find(|s| s.metadata().and_then(|m| m.value.get(key)).is_some())
            .map(|s| s.location)
    }

    /// Keys present in several sources with differing values.
    pub fn conflicts(&self) -> Vec<MetaConflict> { /* pairwise scan over parsed sources */ }

    /// All parsed sources merged into one table, later locations winning.
    /// `Ok(None)` when there is nothing parsed to merge.
    pub fn merged(&self) -> anyhow::Result<Option<Metadata>> { /* fold tables in order */ }

    pub fn get_str(&self, key: &str) -> anyhow::Result<Option<String>> {
        match self.merged()? { Some(m) => m.get_str(key), None => Ok(None) }
    }

    pub fn get_vec_of_string(&self, key: &str) -> anyhow::Result<Option<Vec<String>>> {
        match self.merged()? { Some(m) => m.get_vec_of_string(key), None => Ok(None) }
    }
}
```

`merged()`: start from an empty `toml::Table`; for each `Parsed` source in order, require its value be a table (error naming the location if not) and insert every key, overwriting. `conflicts()`: build a `BTreeMap<String, Vec<(MetaLocation, &toml::Value)>>` across parsed sources, then emit one `MetaConflict` per key whose values are not all equal.

`utils::try_load_file_as_metadata` changes shape — it must no longer propagate the parse error (`entity.rs:278`):

```rust
    /// Loads a sidecar. A file that exists but does not parse yields a `Malformed`
    /// state carrying its raw text and the error, rather than failing the load (D3).
    pub fn try_load_sidecar(
        fs: &dyn Xfs,
        path: &Path,
        location: MetaLocation,
    ) -> anyhow::Result<Option<MetaSource>> {
        let Some(raw) = try_load_file_as_string(fs, path)? else { return Ok(None) };
        let state = match toml::from_str::<toml::Value>(&raw) {
            Ok(value) => MetaState::Parsed(Metadata { value }),
            Err(e) => MetaState::Malformed { raw, error: e.to_string() },
        };
        Ok(Some(MetaSource { location, state, header: None }))
    }
```

Add the matching `utils::parse_header_source(content) -> Option<(MetaSource, String)>` returning the header source plus the body. It must **also** report malformed front matter rather than swallowing it (`parse_header`'s `.ok()?` at `entity.rs:387`/`:397`): when the delimiters are present but the enclosed text does not parse, return a `Malformed` source with the enclosed text as `raw`. When no delimiters are present at all, return `None` — that is plain content, not a broken header.

Update every existing construction and match site. `EntityMeta::Parallel(m)` becomes `EntityMeta::of(vec![MetaSource { location: MetaLocation::ParallelSidecar, state: MetaState::Parsed(m), header: None }])`; add small constructors `EntityMeta::parallel(m)`, `::inside(m)`, `::in_header(m, sep, ht)` preserving the old spellings so the existing ~30 call sites in tests change minimally. `EntityMeta::metadata()` becomes `merged()`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS. Delete the two now-unreachable tests asserting `"Multiple metadata sources found"` and replace them with assertions that both sources merge.

- [ ] **Step 5: Commit**

```bash
git add src/entity.rs
git commit -m "feat: metadata is a list of sources that merge per key

Header and sidecar metadata merge (D2); a source that fails to parse is
retained with its raw text and error instead of aborting the load (D3, C10)."
```

---

## Task 4: `findings.rs` — the diagnostic vocabulary and severity policy

**Files:**
- Create: `src/findings.rs`
- Modify: `src/lib.rs`

**Interfaces:**
- Consumes: `placement::{ContentLocation, Edge, MetaLocation}`, `entity::EntityPath`.
- Produces: `Severity`, `FindingKind`, `FindingKindId`, `Finding`, `FindingPolicy`, `FindingSink`.

This implements **D6**. No I/O — pure data plus the policy lookup.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityPath;
    use crate::placement::{Edge, MetaLocation};

    fn a_stray() -> Finding {
        Finding {
            path: EntityPath::empty().extend_slash("ch1"),
            kind: FindingKind::StrayChild {
                name: "notes".into(),
                edge: Edge::Dot,
                path: "base/ch1.notes.md".into(),
            },
        }
    }

    #[test]
    fn default_policy_is_tolerant_for_drift_and_strict_for_ambiguity() {
        let p = FindingPolicy::default();
        assert_eq!(p.severity(&a_stray().kind), Severity::Warn);
        assert_eq!(
            p.severity(&FindingKind::MetadataKeyConflict {
                key: "type".into(),
                locations: vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar],
            }),
            Severity::Error
        );
        assert_eq!(
            p.severity(&FindingKind::UnexpectedChild { name: "x".into() }),
            Severity::Error
        );
    }

    #[test]
    fn severity_is_configurable_per_kind() {
        let p = FindingPolicy::default().with(FindingKindId::StrayChild, Severity::Error);
        assert_eq!(p.severity(&a_stray().kind), Severity::Error);
        let p = p.with(FindingKindId::StrayChild, Severity::Ignore);
        assert_eq!(p.severity(&a_stray().kind), Severity::Ignore);
    }

    #[test]
    fn strict_preset_makes_everything_an_error() {
        let p = FindingPolicy::strict();
        assert_eq!(p.severity(&a_stray().kind), Severity::Error);
    }

    #[test]
    fn sink_records_warnings_drops_ignored_and_errors_on_error() {
        let mut sink = FindingSink::new(FindingPolicy::default());
        assert!(sink.report(a_stray()).is_ok());
        assert_eq!(sink.findings().len(), 1);

        let mut sink = FindingSink::new(
            FindingPolicy::default().with(FindingKindId::StrayChild, Severity::Ignore),
        );
        assert!(sink.report(a_stray()).is_ok());
        assert!(sink.findings().is_empty());

        let mut sink = FindingSink::new(FindingPolicy::strict());
        let err = sink.report(a_stray()).unwrap_err().to_string();
        assert!(err.contains("notes"), "got: {}", err);
        // An Error finding is still recorded, so a caller inspecting after the failure
        // sees what stopped it.
        assert_eq!(sink.findings().len(), 1);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test findings`
Expected: FAIL — module not declared.

- [ ] **Step 3: Write the implementation**

```rust
//! Drift reporting. §9.1.
//!
//! Two kinds of thing are reported. **Nonconformance** is "this node disagrees with the
//! schema" and has an obvious fix. **Ambiguity** is "this node does not agree with
//! itself" and may need a human. Both travel through the same channel; they are
//! distinguished by kind, and by their default severity.

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

use crate::entity::EntityPath;
use crate::placement::{ContentLocation, Edge, MetaLocation};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Not recorded at all.
    Ignore,
    /// Recorded on the entity / returned by `issues()`.
    Warn,
    /// Recorded, and the call that produced it returns `Err`.
    Error,
}

/// A `FindingKind` with its payload stripped, for use as a policy key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FindingKindId {
    MalformedMetadata,
    MetadataKeyConflict,
    SplitMetadata,
    ContentLocationNonconformance,
    MetadataLocationNonconformance,
    EdgeNonconformance,
    SplitChildEdge,
    StrayContent,
    StrayChild,
    AmbiguousChild,
    MissingRequiredChild,
    MultipleChildrenNotAllowed,
    UnexpectedChild,
    TypeMismatch,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FindingKind {
    /// A metadata source exists but does not parse. C10 / D3.
    MalformedMetadata { location: MetaLocation, path: Option<PathBuf>, error: String },
    /// The same key in two sources with different values. §4.5's unroutable case.
    MetadataKeyConflict { key: String, locations: Vec<MetaLocation> },
    /// Metadata is spread across several sources — ambiguity, not nonconformance.
    SplitMetadata { keys_by_location: Vec<(MetaLocation, Vec<String>)> },
    /// Content is not where the intended layout says. C7 covers the mixed case.
    ContentLocationNonconformance { actual: ContentLocation, intended: ContentLocation },
    /// A sidecar is not where the intended layout says.
    MetadataLocationNonconformance { actual: MetaLocation, intended: MetaLocation },
    /// A child sits on an edge its rule did not declare. §3, C4.
    EdgeNonconformance { name: String, actual: Edge, intended: Edge, rule_index: usize },
    /// One rule's children are split across both edges. §4.2.
    SplitChildEdge { rule_index: usize, slash: Vec<String>, dot: Vec<String> },
    /// The losing half of an `S.md` + `S/content.md` pair. §4.4.
    StrayContent { path: PathBuf, location: ContentLocation },
    /// The losing half of a name found on both edges. §4.4, C2.
    StrayChild { name: String, edge: Edge, path: PathBuf },
    /// A name on both edges that intent could not disambiguate. §3.1, §4.4.
    AmbiguousChild { name: String, dot_path: PathBuf, slash_path: PathBuf },
    /// A `required` rule with no matching child. D1 / C9.
    MissingRequiredChild { rule_index: usize, name_regex: String },
    /// A non-`multiple` rule with several matching children. D1 / C9.
    MultipleChildrenNotAllowed { rule_index: usize, names: Vec<String> },
    /// A child name matching no rule under `allow_additional = false`.
    UnexpectedChild { name: String },
    /// Metadata's `type` key disagrees with the type the parent's rule assigned. §7.1.
    TypeMismatch { expected: String, found: String },
}

impl FindingKind {
    pub fn id(&self) -> FindingKindId { /* one arm per variant */ }
}

impl fmt::Display for FindingKind { /* one human-readable line per variant */ }

/// A finding, with the entity it was observed on.
#[derive(Debug, Clone, PartialEq)]
pub struct Finding {
    pub path: EntityPath,
    pub kind: FindingKind,
}

/// Per-kind severity. Tolerant by default; ambiguity that has no defined resolution
/// defaults to `Error`.
#[derive(Debug, Clone)]
pub struct FindingPolicy {
    overrides: HashMap<FindingKindId, Severity>,
}

impl Default for FindingPolicy {
    fn default() -> Self { FindingPolicy { overrides: HashMap::new() } }
}

impl FindingPolicy {
    pub fn severity(&self, kind: &FindingKind) -> Severity {
        if let Some(s) = self.overrides.get(&kind.id()) { return *s; }
        match kind.id() {
            // No defined resolution without a human, or a genuine schema violation.
            FindingKindId::MetadataKeyConflict
            | FindingKindId::AmbiguousChild
            | FindingKindId::UnexpectedChild
            | FindingKindId::TypeMismatch => Severity::Error,
            // Everything else is drift: normal in a tree humans edit by hand.
            _ => Severity::Warn,
        }
    }

    #[must_use]
    pub fn with(mut self, id: FindingKindId, severity: Severity) -> Self {
        self.overrides.insert(id, severity);
        self
    }

    /// Every kind is an error. For CI and scripted batch runs.
    pub fn strict() -> Self { /* insert Error for every FindingKindId */ }

    /// Nothing is recorded. For consumers that only want the data.
    pub fn silent() -> Self { /* insert Ignore for every FindingKindId */ }
}

/// Accumulates findings for one node, applying the policy as each is reported.
#[derive(Debug)]
pub struct FindingSink {
    policy: FindingPolicy,
    findings: Vec<Finding>,
}

impl FindingSink {
    pub fn new(policy: FindingPolicy) -> Self { FindingSink { policy, findings: vec![] } }

    /// Records the finding per policy. Returns `Err` if its severity is `Error` — the
    /// finding is still recorded first, so the caller can inspect what stopped it.
    pub fn report(&mut self, f: Finding) -> anyhow::Result<()> {
        match self.policy.severity(&f.kind) {
            Severity::Ignore => Ok(()),
            Severity::Warn => { self.findings.push(f); Ok(()) }
            Severity::Error => {
                let msg = format!("{} at {:?}", f.kind, f.path.local_path());
                self.findings.push(f);
                Err(anyhow::anyhow!(msg))
            }
        }
    }

    /// Records regardless of severity and never errors. Used by `issues()`, whose whole
    /// job is to report rather than to enforce.
    pub fn observe(&mut self, f: Finding) { self.findings.push(f); }

    pub fn findings(&self) -> &[Finding] { &self.findings }
    pub fn into_findings(self) -> Vec<Finding> { self.findings }
    pub fn policy(&self) -> &FindingPolicy { &self.policy }
}
```

Add `pub mod findings;` to `src/lib.rs`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test findings`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add src/findings.rs src/lib.rs
git commit -m "feat: add findings module with per-kind severity policy

Implements D6: tolerant by default, configurable per finding kind,
strict() and silent() presets."
```

---

## Task 5: `discovery.rs` — one child resolver, with cross-pass dedup and intent tie-breaking

**Files:**
- Create: `src/discovery.rs`
- Modify: `src/entity.rs` (move `find_dot_children`/`find_slash_children` out of `utils`), `src/lib.rs`

**Interfaces:**
- Consumes: `placement::{Edge, RESERVED_*}`, `schema::{CompiledType, ChildMatch}`, `findings::{Finding, FindingKind, FindingSink}`.
- Produces: `ResolvedChild { name, path, edge, node_type: Option<String>, rule_index: Option<usize> }`, `resolve_children(fs, base_path, path, ctype, sink) -> Result<Vec<ResolvedChild>>`.

This is the fix for **C2** (load and lookup disagree about a name on both edges), **C8** (a matched rule with no files behind it errors), and §4.2's per-rule edge observation. Both `EntityLoader` and `LiveEntity` call it, which is what makes them agree by construction.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityPath;
    use crate::findings::{FindingKind, FindingPolicy, FindingSink};
    use crate::placement::Edge;
    use crate::schema::{ChildEntityRules, EntityTypeDescription, Schema};
    use inscenerator_xfs::mockfs;
    use std::path::{Path, PathBuf};

    fn fs_with(files: &[(&str, &str)]) -> mockfs::MockFS {
        let mut fs = mockfs::MockFS::new();
        for (p, c) in files {
            let p = PathBuf::from(p);
            fs.create_dir_all(p.parent().unwrap()).unwrap();
            fs.add_r(&p, c.as_bytes().to_vec()).unwrap();
        }
        fs
    }

    fn schema_with_rule(name_regex: &str, edge: Edge, allow_additional: bool) -> Schema {
        let mut s = Schema::new();
        s.add_entity_type(EntityTypeDescription {
            name: "T".into(),
            children: vec![ChildEntityRules {
                name_regex: name_regex.into(),
                node_type: "T".into(),
                required: false,
                multiple: true,
                edge,
            }],
            allow_additional,
            ignore: vec![],
            layout: None,
        })
        .unwrap();
        s
    }

    fn resolve(
        fs: &mockfs::MockFS,
        schema: &Schema,
        sink: &mut FindingSink,
    ) -> anyhow::Result<Vec<ResolvedChild>> {
        resolve_children(
            fs,
            Path::new("base"),
            &EntityPath::empty().extend_slash("ch1"),
            schema.compiled("T").unwrap(),
            sink,
        )
    }

    /// C2: the same name on both edges yields exactly ONE child — the one matching the
    /// rule's intended edge — and the other is reported as a stray.
    #[test]
    fn duplicate_name_resolves_to_the_intended_edge() {
        let fs = fs_with(&[
            ("base/ch1.notes.md", "dot"),
            ("base/ch1/notes.md", "slash"),
        ]);
        let schema = schema_with_rule("^notes$", Edge::Slash, false);
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(kids.len(), 1);
        assert_eq!(kids[0].name, "notes");
        assert_eq!(kids[0].edge, Edge::Slash);

        let strays: Vec<_> = sink
            .findings()
            .iter()
            .filter(|f| matches!(f.kind, FindingKind::StrayChild { .. }))
            .collect();
        assert_eq!(strays.len(), 1);
        match &strays[0].kind {
            FindingKind::StrayChild { name, edge, path } => {
                assert_eq!(name, "notes");
                assert_eq!(*edge, Edge::Dot);
                assert_eq!(path, &PathBuf::from("base/ch1.notes"));
            }
            _ => unreachable!(),
        }
    }

    /// C2 second half: with no rule to declare an edge, intent cannot break the tie.
    #[test]
    fn duplicate_name_with_no_rule_is_ambiguous_and_errors() {
        let fs = fs_with(&[
            ("base/ch1.notes.md", "dot"),
            ("base/ch1/notes.md", "slash"),
        ]);
        let schema = schema_with_rule("^never-matches$", Edge::Slash, true);
        let mut sink = FindingSink::new(FindingPolicy::default());
        let err = resolve(&fs, &schema, &mut sink).unwrap_err().to_string();
        assert!(err.contains("ch1.notes"), "error must name both paths: {}", err);
        assert!(err.contains("ch1/notes"), "error must name both paths: {}", err);
    }

    /// §3: a child found on the wrong edge still loads, and is reported.
    #[test]
    fn wrong_edge_still_loads_and_is_reported() {
        let fs = fs_with(&[("base/ch1.notes.md", "dot")]);
        let schema = schema_with_rule("^notes$", Edge::Slash, false);
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(kids.len(), 1);
        assert_eq!(kids[0].edge, Edge::Dot);
        assert!(sink.findings().iter().any(|f| matches!(
            &f.kind,
            FindingKind::EdgeNonconformance { name, actual: Edge::Dot, intended: Edge::Slash, .. }
                if name == "notes"
        )));
    }

    /// §4.2: two rules with different edges are not ambiguous — the split is per rule.
    #[test]
    fn children_on_two_edges_under_two_rules_are_not_split() {
        let fs = fs_with(&[
            ("base/ch1/010-intro.md", "s"),
            ("base/ch1.notes.md", "n"),
        ]);
        let mut schema = Schema::new();
        schema
            .add_entity_type(EntityTypeDescription {
                name: "T".into(),
                children: vec![
                    ChildEntityRules {
                        name_regex: r"^\d{3}-".into(), node_type: "T".into(),
                        required: false, multiple: true, edge: Edge::Slash,
                    },
                    ChildEntityRules {
                        name_regex: "^notes$".into(), node_type: "T".into(),
                        required: false, multiple: true, edge: Edge::Dot,
                    },
                ],
                allow_additional: false,
                ignore: vec![],
                layout: None,
            })
            .unwrap();
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve(&fs, &schema, &mut sink).unwrap();
        assert_eq!(kids.len(), 2);
        assert!(!sink
            .findings()
            .iter()
            .any(|f| matches!(f.kind, FindingKind::SplitChildEdge { .. })));
    }

    /// §4.2: one rule whose children sit on both edges IS split.
    #[test]
    fn one_rule_split_across_both_edges_is_reported() {
        let fs = fs_with(&[
            ("base/ch1/010-intro.md", "s"),
            ("base/ch1.020-body.md", "d"),
        ]);
        let schema = schema_with_rule(r"^\d{3}-", Edge::Slash, false);
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve(&fs, &schema, &mut sink).unwrap();
        assert_eq!(kids.len(), 2);
        assert!(sink.findings().iter().any(|f| matches!(
            &f.kind,
            FindingKind::SplitChildEdge { slash, dot, .. }
                if slash == &vec!["010-intro".to_string()] && dot == &vec!["020-body".to_string()]
        )));
    }

    /// C8: a name matching a rule but with nothing behind it is not an error.
    #[test]
    fn matched_rule_with_no_files_is_not_an_error() {
        let fs = fs_with(&[("base/ch1/.gitkeep", "")]);
        let schema = schema_with_rule(r"^\.gitkeep$", Edge::Slash, false);
        let mut sink = FindingSink::new(FindingPolicy::default());
        // Resolution succeeds and names the candidate; whether it becomes an entity is
        // the loader's call (Task 6), which drops it when try_load returns None.
        let kids = resolve(&fs, &schema, &mut sink).unwrap();
        assert_eq!(kids.len(), 1);
        assert!(sink
            .findings()
            .iter()
            .all(|f| !matches!(f.kind, FindingKind::UnexpectedChild { .. })));
    }

    #[test]
    fn required_and_multiple_are_reported_not_enforced() {
        let fs = fs_with(&[("base/ch1/a.md", ""), ("base/ch1/b.md", "")]);
        let mut schema = Schema::new();
        schema
            .add_entity_type(EntityTypeDescription {
                name: "T".into(),
                children: vec![
                    ChildEntityRules {
                        name_regex: "^[ab]$".into(), node_type: "T".into(),
                        required: false, multiple: false, edge: Edge::Slash,
                    },
                    ChildEntityRules {
                        name_regex: "^zzz$".into(), node_type: "T".into(),
                        required: true, multiple: true, edge: Edge::Slash,
                    },
                ],
                allow_additional: false,
                ignore: vec![],
                layout: None,
            })
            .unwrap();
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve(&fs, &schema, &mut sink).unwrap();
        assert_eq!(kids.len(), 2, "both children still load");
        assert!(sink.findings().iter().any(|f| matches!(
            f.kind, FindingKind::MultipleChildrenNotAllowed { rule_index: 0, .. }
        )));
        assert!(sink.findings().iter().any(|f| matches!(
            f.kind, FindingKind::MissingRequiredChild { rule_index: 1, .. }
        )));
    }

    #[test]
    fn root_has_no_dot_children() {
        let fs = fs_with(&[("base.notes.md", "x"), ("base/a.md", "")]);
        let schema = schema_with_rule("^a$", Edge::Slash, false);
        let mut sink = FindingSink::new(FindingPolicy::default());
        let kids = resolve_children(
            &fs, Path::new("base"), &EntityPath::empty(),
            schema.compiled("T").unwrap(), &mut sink,
        ).unwrap();
        assert_eq!(kids.len(), 1);
        assert_eq!(kids[0].name, "a");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test discovery`
Expected: FAIL — module not declared.

- [ ] **Step 3: Write the implementation**

Move `find_dot_children` and `find_slash_children` from `entity.rs::utils` (`:182-259`) into `src/discovery.rs`, changing them to return `Vec<String>` of **names** rather than `Vec<EntityPath>` — the edge is decided later, so producing paths here is premature. Keep their skip lists, now referencing `placement::RESERVED_DOT_SUFFIXES` and `placement::RESERVED_SLASH_NAMES`.

```rust
/// A child, after both passes have been reconciled against the schema's intent.
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedChild {
    pub name: String,
    /// The logical path, on the edge that won.
    pub path: EntityPath,
    /// The edge this child actually sits on — which may not be the rule's intended edge.
    pub edge: Edge,
    /// The type from the matching rule. `None` for an `allow_additional` child, which
    /// has no rule and therefore no declared type (§7.2).
    pub node_type: Option<String>,
    /// Index of the rule that matched, for per-rule edge observation (§4.2).
    pub rule_index: Option<usize>,
}

pub fn resolve_children(
    fs: &dyn Xfs,
    base_path: &Path,
    path: &EntityPath,
    ctype: &CompiledType,
    sink: &mut FindingSink,
) -> anyhow::Result<Vec<ResolvedChild>>
```

Algorithm:

1. `is_root = path.entries.is_empty()`. Run the dot pass only when `!is_root` (§3, the root has no filename to prefix). Run the slash pass always.
2. Merge both name lists into a `BTreeMap<String, (bool /*dot*/, bool /*slash*/)>`. This is the **cross-pass dedup** that `entity.rs:551-554` currently lacks — a name is never resolved twice.
3. For each `(name, (dot, slash))`, in name order:
   - `match ctype.match_child(&name)`:
     - `Ignored` → skip entirely.
     - `Unexpected` → `sink.report(UnexpectedChild { name })?`, then skip.
     - `Matched(rule)` → `intended = Some(rule.rule.edge)`, `node_type = Some(rule.rule.node_type)`, `rule_index = Some(rule.index)`.
     - `Additional` → `intended = None` (no rule, so no declared edge — §7.2), `node_type = None`, `rule_index = None`.
   - Pick the edge:
     - both present, `intended = Some(e)` → `edge = e`; `sink.report(StrayChild { name, edge: other, path: stem(base, child_path(path, name, other)) })?`.
     - both present, `intended = None` → `sink.report(AmbiguousChild { name, dot_path, slash_path })?`. If the policy downgraded it, fall through with `edge = Edge::Slash` (documented deterministic fallback).
     - only one present → `edge` is that one. If `intended = Some(e)` and `e != edge`, `sink.report(EdgeNonconformance { name, actual: edge, intended: e, rule_index })?`.
   - Push a `ResolvedChild`.
4. After the loop, group the resolved children by `rule_index`. For each rule index present on both edges, `sink.report(SplitChildEdge { rule_index, slash, dot })?` with the names sorted.
5. For each rule in `ctype.rules`: count children with that `rule_index`. `count == 0 && rule.required` → `MissingRequiredChild`. `count > 1 && !rule.multiple` → `MultipleChildrenNotAllowed { names }`.

Add `pub mod discovery;` to `src/lib.rs`. Have `entity.rs` and `live_entity.rs` call `discovery::resolve_children` in place of their inline loops so the crate still compiles; Tasks 6 and 8 finish those call sites.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: discovery's 8 tests PASS; whole suite green.

- [ ] **Step 5: Commit**

```bash
git add src/discovery.rs src/entity.rs src/live_entity.rs src/lib.rs
git commit -m "feat: one child resolver with cross-pass dedup and intent tie-breaking

Fixes C2 (Entity yielded two children for one name while child() refused
to resolve it) and C8 (a matched rule with no files behind it errored).
Adds per-rule edge observation and required/multiple reporting (D1)."
```

---

## Task 6: `EntityLoader` — tolerant reads, layout inheritance, findings on the entity

**Files:**
- Modify: `src/entity.rs` (`Entity` struct at `:801`, `EntityLoader::try_load_entity` at `:461-637`)

**Interfaces:**
- Consumes: everything from Tasks 1–5.
- Produces: `Entity.findings: Vec<Finding>`, `Entity.layout: Layout`, `Entity::all_findings()`, `EntityLoader.policy: FindingPolicy`, `EntityLoader::with_policy()`.

This implements **§2.1** (layout inherited by instance), **§4.4** (intent disambiguates content rather than rejecting), **C7** (mixed layouts load and are reported), and **D4**'s eager half.

- [ ] **Step 1: Write the failing tests**

Add to `entity.rs`'s `mod tests`. `dummy_loader` (`:836-860`) stays as it is for the existing tests; add a second fixture beside it so layout inheritance is observable — `TestType` declares `Parallel`, `ChildTestType` declares nothing and therefore inherits:

```rust
    fn parallel_loader() -> EntityLoader {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "TestType".into(),
            children: vec![ChildEntityRules {
                name_regex: ".*".into(), node_type: "ChildTestType".into(),
                required: false, multiple: true, edge: Edge::Slash,
            }],
            allow_additional: false, ignore: vec![],
            layout: Some(Layout::Parallel),
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "ChildTestType".into(), children: vec![], allow_additional: true,
            ignore: vec![], layout: None,     // inherits from the parent instance
        }).unwrap();
        let mut loader = EntityLoader::new();
        loader.schema = schema;
        loader
    }
```

```rust
    /// §4.4: `S.md` + `S/content.md` no longer errors — intent picks, the other is a stray.
    #[test]
    fn both_content_files_resolve_by_intended_layout() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "e.md", "parallel body");
        create_file_with_content(&mut fs, "foo/e", "content.md", "inside body");

        let loader = parallel_loader(); // TestType declares layout = "parallel"
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty().extend_slash("e"), "TestType")
            .unwrap()
            .unwrap();

        assert_eq!(e.content, EntityContent::Parallel("parallel body".into()));
        assert!(e.findings.iter().any(|f| matches!(
            &f.kind,
            FindingKind::StrayContent { location: ContentLocation::Inside, .. }
        )));
    }

    /// §2.1: a type that omits `layout` inherits from the parent instance.
    #[test]
    fn layout_is_inherited_by_instance() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "e.md", "parent");
        create_file_with_content(&mut fs, "foo/e", "kid.md", "child");

        let loader = parallel_loader();
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty().extend_slash("e"), "TestType")
            .unwrap()
            .unwrap();
        assert_eq!(e.layout, Layout::Parallel);
        assert_eq!(e.children.len(), 1);
        // ChildTestType declares no layout, so it inherits Parallel from `e`.
        assert_eq!(e.children[0].layout, Layout::Parallel);
        assert!(e.children[0].findings.is_empty());
    }

    #[test]
    fn root_is_always_inside() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "content.md", "root body");
        create_file_with_content(&mut fs, "foo", "meta.toml", "");
        let loader = parallel_loader();
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty(), "TestType")
            .unwrap()
            .unwrap();
        assert_eq!(e.layout, Layout::Inside);
    }

    /// C7: a mixed layout loads with correct content and metadata, and is reported.
    #[test]
    fn mixed_layout_loads_and_is_reported() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "e.md", "body");
        create_file_with_content(&mut fs, "foo/e", "meta.toml", "k = 1");

        let loader = parallel_loader();
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty().extend_slash("e"), "TestType")
            .unwrap()
            .unwrap();
        assert_eq!(e.content.content(), Some("body"));
        assert_eq!(e.metadata.merged().unwrap().unwrap().value.get("k").unwrap().as_integer(), Some(1));
        assert!(e.findings.iter().any(|f| matches!(
            &f.kind,
            FindingKind::MetadataLocationNonconformance {
                actual: MetaLocation::InsideSidecar,
                intended: MetaLocation::ParallelSidecar,
            }
        )));
    }

    /// C10 / D3: a malformed sidecar no longer aborts the load.
    #[test]
    fn malformed_sidecar_is_reported_not_fatal() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "e.md", "body");
        create_file_with_content(&mut fs, "foo", "e.meta.toml", "this = = not toml");

        let loader = parallel_loader();
        let e = loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty().extend_slash("e"), "TestType")
            .unwrap()
            .unwrap();
        assert_eq!(e.content.content(), Some("body"));
        assert_eq!(e.metadata.malformed().len(), 1);
        assert!(e.findings.iter().any(|f| matches!(f.kind, FindingKind::MalformedMetadata { .. })));
    }

    /// D6: the same tree fails up-front under a strict policy.
    #[test]
    fn strict_policy_fails_the_load() {
        let mut fs = mockfs::MockFS::new();
        create_file_with_content(&mut fs, "foo", "e.md", "body");
        create_file_with_content(&mut fs, "foo", "e.meta.toml", "this = = not toml");

        let loader = parallel_loader().with_policy(FindingPolicy::strict());
        assert!(loader
            .try_load_entity(&fs, &PathBuf::from("foo"), &EntityPath::empty().extend_slash("e"), "TestType")
            .is_err());
    }

    #[test]
    fn all_findings_walks_the_tree() {
        // Build a root with one nonconforming grandchild; assert all_findings()
        // returns it with the grandchild's EntityPath attached.
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib entity`
Expected: FAIL — `Entity.findings`, `Entity.layout`, `with_policy` do not exist.

- [ ] **Step 3: Write the implementation**

`Entity` gains two fields:

```rust
#[derive(Debug, PartialEq, Clone)]
pub struct Entity {
    pub path: EntityPath,
    pub node_type: String,
    pub content: EntityContent,
    pub metadata: EntityMeta,
    pub children: Vec<Entity>,
    /// The layout this node was resolved under — declared by its type, or inherited
    /// from the parent instance it was loaded beneath (§2.1). Used when writing.
    pub layout: Layout,
    /// Drift observed on *this* node. Children carry their own. §9.1.
    pub findings: Vec<Finding>,
}

impl Entity {
    /// This node's findings and every descendant's, depth-first.
    pub fn all_findings(&self) -> Vec<Finding> { /* recurse */ }
}
```

`EntityLoader` gains `pub policy: FindingPolicy` and `#[must_use] pub fn with_policy(mut self, p: FindingPolicy) -> Self`.

`try_load_entity` gains an `inherited_layout: Layout` parameter. Add a thin public entry point so callers do not have to know about it:

```rust
    /// Loads the root of a tree. The root is always `Inside` (§2).
    pub fn try_load_root(&self, fs: &dyn Xfs, base_path: &Path, entity_type: &str)
        -> anyhow::Result<Option<Entity>> {
        self.try_load_entity(fs, base_path, &EntityPath::empty(), entity_type, Layout::Inside)
    }
```

Rewrite the body in this order:

1. **Root checks** — unchanged from `:469-476`.
2. **Probe content at both locations.** Replace `:478-498`. Build both paths via `placement::content_path`. Load whichever exist.
   - Both exist → `intended_layout` (computed at step 4 — so defer the tie-break until after typing, or compute intent from the *inherited* layout when the type is not yet known and refine after; simplest correct order is: probe existence only, resolve type, then choose). Choose the file matching `intended.content_location()`; `sink.report(StrayContent { path, location: other })?`.
   - Only one exists → that is the content, and if its location differs from `intended.content_location()`, `sink.report(ContentLocationNonconformance { .. })?`.
   - Root: only `Inside` is probed.
3. **Parse the header** into a `MetaSource` via `utils::parse_header_source`, keeping the body as the content string.
4. **Probe both sidecars** via `placement::sidecar_path` — note `ParallelSidecar` is now `stem.with_added_extension("meta.toml")`, which is the **C1 reader fix**, replacing `:514-516`. Skip the parallel sidecar for the root. Build `EntityMeta::of(sources)`; delete the `meta_sources.len() > 1` bail at `:536-541`.
   - For each malformed source → `MalformedMetadata` finding.
   - For each conflict from `meta.conflicts()` → `MetadataKeyConflict` finding.
   - If more than one source → `SplitMetadata` finding, listing each location's keys.
   - For each **sidecar** source whose location is not `intended.sidecar_location()` → `MetadataLocationNonconformance`. `InHeader` is never nonconforming.
5. **Discover children** via `discovery::resolve_children`, passing the same sink.
6. **Existence check** — unchanged semantics (§6): return `Ok(None)` when there is no content, no metadata source, no children, and no directory.
7. **Resolve the type**, as at `:562-592`, but reading via `metadata.get_str("type")` and reporting a mismatch as a `TypeMismatch` finding (default `Error`, so existing behaviour is preserved) rather than a bare `bail!`.
8. **Compute this node's layout:** `let layout = if is_root { Layout::Inside } else { ctype.desc.layout.unwrap_or(inherited_layout) };`. If `is_root && ctype.desc.layout == Some(Layout::Parallel)`, `bail!` — that is a schema error, not drift (§2). Likewise `bail!` if `is_root` and any of the root type's rules declares `edge = "dot"` (§3).
9. **Load each resolved child**, passing `layout` down as the child's `inherited_layout` and `child.node_type` (or the caller-supplied `with_type` equivalent — for `ResolvedChild { node_type: None }`, use `"Auto"`). A child that loads as `None` is simply skipped (**C8**).
10. Build the `Entity` with `layout` and `sink.into_findings()`.

Note the ordering wrinkle at step 2: the intended layout is not known until step 8, but the content tie-break at step 2 needs it. Resolve by probing existence in step 2, deferring the *choice* until after step 8, then reading the chosen file. Write it that way — probe, type, decide, read.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS. Update the tests that asserted `"Both ... exist."` and `"Multiple metadata sources found"` to the new tolerant behaviour.

- [ ] **Step 5: Commit**

```bash
git add src/entity.rs
git commit -m "feat: tolerant loader with layout inheritance and per-node findings

Implements 2.1, 4.4 and D4's eager half; fixes C1 (reader), C7 (mixed
layouts load and report) and C10 (malformed sidecar no longer fatal)."
```

---

## Task 7: `EntityWriter` — write through `placement`

**Files:**
- Modify: `src/entity.rs` (`EntityWriter::write_entity` at `:649-710`)

**Interfaces:**
- Consumes: `Entity.layout`, `placement::{content_path, sidecar_path}`, `EntityMeta::sources()`.

This is the **C1 writer fix** — the substitution at `:693` is what makes a dot-child overwrite its parent's sidecar, and children are written *after* the parent, so it lands every time.

- [ ] **Step 1: Write the failing test**

```rust
    /// C1: writing a dot-child with a parallel sidecar must not touch its parent's.
    #[test]
    fn writing_a_dot_child_does_not_clobber_the_parents_sidecar() {
        let parent = Entity {
            path: EntityPath::empty().extend_slash("ch1"),
            node_type: "TestType".into(),
            content: EntityContent::Parallel("chapter body".into()),
            metadata: EntityMeta::parallel(Metadata { value: toml::from_str("owner = \"parent\"").unwrap() }),
            children: vec![Entity {
                path: EntityPath::empty().extend_slash("ch1").extend_dot("review"),
                node_type: "ChildTestType".into(),
                content: EntityContent::Parallel("review body".into()),
                metadata: EntityMeta::parallel(Metadata { value: toml::from_str("owner = \"child\"").unwrap() }),
                children: vec![],
                layout: Layout::Parallel,
                findings: vec![],
            }],
            layout: Layout::Parallel,
            findings: vec![],
        };

        let mut fs = mockfs::MockFS::new();
        fs.create_dir_all(&PathBuf::from("foo")).unwrap();
        EntityWriter {}.write_entity(&mut fs, &PathBuf::from("foo"), &parent).unwrap();

        let read = |p: &str| {
            utils::try_load_file_as_string(&fs, &PathBuf::from(p)).unwrap().unwrap()
        };
        assert!(read("foo/ch1.meta.toml").contains("parent"));
        assert!(read("foo/ch1.review.meta.toml").contains("child"));
        assert_eq!(read("foo/ch1.md"), "chapter body");
        assert_eq!(read("foo/ch1.review.md"), "review body");
    }

    /// C1 round-trip: what the writer writes, the loader reads back.
    #[test]
    fn dot_child_parallel_sidecar_round_trips() {
        // write the tree above, then EntityLoader::try_load_root and assert the
        // child's metadata is owner="child" and the parent's is owner="parent",
        // with no findings on either.
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib writing_a_dot_child`
Expected: FAIL — `foo/ch1.meta.toml` contains `owner = "child"`, because `with_extension` collapsed the child onto the parent.

- [ ] **Step 3: Write the implementation**

Rewrite `write_entity`:

- `needs_directory` becomes `is_empty || entity.layout == Layout::Inside || any child is on the slash edge` — read from `entity.layout` and the children's last entries, not from probing the content/metadata variants.
- Content path: `placement::content_path(base_path, &entity.path, entity.layout.content_location())` — replacing the `match &entity.content` at `:677-681`. If the entity's `EntityContent` variant disagrees with `entity.layout`, honour the variant (it records where the content actually is); the layout is only the fallback for `EntityContent::None`.
- Header: if any source has `location == MetaLocation::InHeader`, prepend `utils::format_metadata_header(...)` using its `HeaderInfo`.
- Sidecars: iterate `entity.metadata.sources()`, skipping `InHeader`, and write each to `placement::sidecar_path(base_path, &entity.path, source.location).unwrap()`. This both fixes `:693` and makes a merged node round-trip — today the writer can only emit one source.
- A `Malformed` source is written back **verbatim from its `raw`**, so a load/save cycle never destroys a file the library could not parse.
- Children recursion unchanged.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/entity.rs
git commit -m "fix: writer appends the sidecar suffix instead of substituting (C1)

A dot-child with a parallel sidecar no longer overwrites its parent's
metadata. Merged and malformed sources now round-trip."
```

---

## Task 8: `LiveEntity` read side — placement observation, `issues()`, agreement with the loader

**Files:**
- Modify: `src/live_entity.rs` (`LiveEntity` struct `:32-40`, `LiveEntityRoot` `:14-21`, path helpers `:533-555`, `get_content_info` `:561`, `content` `:593`, `metadata` `:617`, `children` `:666`, `child` `:991`)

**Interfaces:**
- Produces: `LiveEntity.inherited_layout`, `LiveEntity::intended_layout()`, `::observed()`, `::issues()`, `LiveEntityRoot.policy`, `ObservedPlacement`.

This implements **D4**'s lazy half and makes `child()` and `children()` agree with each other and with `EntityLoader` (**C2**).

- [ ] **Step 1: Write the failing tests**

Two helpers, beside the existing `setup_schema` (`live_entity.rs:1056-1070`):

```rust
    /// A schema whose `notes` rule declares the slash edge, for the C2 pair.
    fn edge_schema_slash() -> Arc<Schema> {
        let mut s = Schema::new();
        s.add_entity_type(EntityTypeDescription {
            name: "Type".into(),
            children: vec![ChildEntityRules {
                name_regex: "^notes$".into(), node_type: "Type".into(),
                required: false, multiple: false, edge: Edge::Slash,
            }],
            allow_additional: true, ignore: vec![], layout: None,
        }).unwrap();
        Arc::new(s)
    }

    /// A handle on `foo/parent`, typed `Type`.
    fn live_at(fs: Arc<Mutex<mockfs::MockFS>>, schema: Arc<Schema>) -> LiveEntity {
        LiveEntity::new(
            fs, PathBuf::from("foo"),
            EntityPath::empty().extend_slash("parent"),
            "Type".to_string(), schema, FindingPolicy::default(),
        )
    }
```

```rust
    /// C2: load and lookup must agree. This is the pair that contradicted each other.
    #[test]
    fn child_and_children_agree_on_a_duplicated_name() {
        let mut raw = mockfs::MockFS::new();
        create_file_with_content(&mut raw, "foo/parent", "notes.md", "slash");
        create_file_with_content(&mut raw, "foo", "parent.notes.md", "dot");
        let fs = Arc::new(Mutex::new(raw));
        let live = live_at(fs, edge_schema_slash()); // rule: notes, edge = "slash"

        let kids = live.children().unwrap();
        assert_eq!(kids.len(), 1);
        assert_eq!(kids[0].path, live.path.extend_slash("notes"));

        let one = live.child("notes").unwrap();
        assert_eq!(one.path, kids[0].path);
    }

    #[test]
    fn issues_reports_the_stray_without_failing_the_read() {
        // same tree; assert issues() contains StrayChild and that content()/metadata()
        // both succeed.
    }

    #[test]
    fn metadata_merges_header_and_sidecar() {
        // foo/e.md with a toml header `type = "T"`, foo/e.meta.toml with `n = 1`.
        // metadata().merged() has both keys; location_of routes each correctly.
    }

    #[test]
    fn intended_layout_is_inherited_from_the_parent_handle() {
        // parent type declares layout = "parallel"; child type declares nothing.
        // assert live.child("kid").unwrap().intended_layout().unwrap() == Layout::Parallel
    }

    #[test]
    fn error_severity_finding_fails_only_the_accessor_that_produced_it() {
        // A tree with a malformed sidecar under FindingPolicy::strict():
        // metadata() returns Err, but children() still succeeds.
    }

    /// D3 / C10, matching Task 6's loader: under the default policy a malformed sidecar
    /// does not stop `metadata()` returning the node's sources, but it does stop a value
    /// being *read* out of them — "absent" would be a guess about a file we could not read.
    #[test]
    fn a_malformed_source_is_reachable_but_not_readable_through() {
        // foo/e.md with a good header `type = "T"`, foo/e.meta.toml holding "x = = 1".
        let meta = live.metadata().unwrap();          // the probe itself succeeds
        assert!(meta.get_str("type").is_err());       // reading through it does not
        let bad = meta.malformed();                   // ...but the caller can reach it
        assert_eq!(bad.len(), 1);
        assert_eq!(bad[0].describe(), "e.meta.toml", "named as the user would name it");
        assert!(bad[0].raw().unwrap().contains("x = = 1"));
        assert!(bad[0].error().unwrap().contains("expected"));
        // The good source is still individually legible, for a caller that wants to
        // reconstruct the bad one from what survived.
        let good = meta.source_at(MetaLocation::InHeader).unwrap();
        assert_eq!(good.metadata().unwrap().get_str("type").unwrap().as_deref(), Some("T"));
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib live_entity`
Expected: FAIL.

- [ ] **Step 3: Write the implementation**

`LiveEntityRoot` gains `pub policy: FindingPolicy`. `LiveEntity` gains:

```rust
pub struct LiveEntity {
    pub root: Arc<LiveEntityRoot>,
    pub path: EntityPath,
    pub node_type: String,
    /// The layout inherited from the parent handle this one was reached through (§2.1).
    /// `Layout::Inside` for a root or a handle built directly via `new()`.
    pub inherited_layout: Layout,
}
```

`inherited_layout` is *static* — derived from schema and ancestry, never from disk — so it does not go stale the way a disk snapshot would.

`LiveEntity::new` (`:452-468`) gains a trailing `policy: FindingPolicy` parameter and sets `inherited_layout: Layout::Inside` — a handle built directly by address has no parent to inherit from, and `Inside` is the root's layout. `load_from_root` (`:475-485`) passes `FindingPolicy::default()`; add `load_from_root_with_policy` for callers that want strictness. Every existing `LiveEntity::new` call site in tests needs the extra argument.

```rust
    /// The layout this node's type intends, falling back to what it inherited (§2.1).
    /// Always `Inside` for the root.
    pub fn intended_layout(&self) -> anyhow::Result<Layout> {
        if self.path.entries.is_empty() { return Ok(Layout::Inside); }
        let t = self.actual_type()?;
        Ok(self.root.schema.get_entity_type(&t)?.layout.unwrap_or(self.inherited_layout))
    }
```

Add the observation type and probe:

```rust
/// What the node has actually established on disk. §4.1. Used only for creating and
/// editing — nothing in the read semantics of an entity depends on it.
#[derive(Debug, Clone, PartialEq)]
pub struct ObservedPlacement {
    /// `None` when no content file exists — defer to intent.
    pub content: Option<ContentLocation>,
    /// The metadata sources present, in `MetaLocation` order. Empty means none.
    pub metadata: Vec<MetaLocation>,
}

impl LiveEntity {
    pub fn observed(&self) -> anyhow::Result<ObservedPlacement> { /* probe both locations */ }

    /// Everything §9.1 reports about *this* node, computed from disk at call time.
    /// Never returns `Err` for a finding, whatever the policy says — reporting is this
    /// method's whole job. It errors only on I/O failure.
    pub fn issues(&self) -> anyhow::Result<Vec<Finding>> { /* see below */ }
}
```

Rewrite the path helpers `:533-555` to delegate to `placement` — in particular `dot_metadata_path` at `:548-550` currently uses `with_extension`, the third C1 site.

`content()`: probe both; if both exist, pick by `intended_layout()` and report `StrayContent` through a sink; if one exists on the non-intended side, report `ContentLocationNonconformance`. Delete the `bail!` at `:574-579`.

`metadata()`: build `EntityMeta::of(...)` from all present sources exactly as Task 6's loader does, reporting `MalformedMetadata`, `MetadataKeyConflict`, `SplitMetadata` and `MetadataLocationNonconformance` through a sink whose `report` errors under an `Error` severity. Delete the `bail!` at `:651-656`.

`children()`: call `discovery::resolve_children` with a sink; construct each `LiveEntity` with `inherited_layout: self.intended_layout()?` and `node_type` from `ResolvedChild.node_type.unwrap_or("Auto")`.

`child(name)`: implement as `self.children()?.into_iter().find(|c| c.path.last_name() == Some(name))`, erroring `"No child found with name '{name}'"` when absent. Doing the full resolution is what *guarantees* the agreement C2 demands — it is the same code path, not a parallel one. Delete the `(true, true) => bail!` at `:1008` and both `child_type` call sites at `:1011`/`:1020`.

`issues()`: run the content probe, the metadata probe and `resolve_children` against a `FindingSink::new(FindingPolicy::default())` using `observe()` rather than `report()`, so every finding is collected and none short-circuits, then return them.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/live_entity.rs
git commit -m "feat: LiveEntity reads tolerantly and reports via issues()

child() now resolves through the same code as children(), so load and
lookup can no longer disagree (C2). Fixes the third C1 substitution site."
```

---

## Task 9: `LiveEntity` write side — route to what is already there

**Files:**
- Modify: `src/live_entity.rs` (`set_content` `:723`, `set_metadata` `:759`, `delete` `:832`)

**Interfaces:**
- Produces: `set_meta_key`, `remove_meta_key`, `set_metadata_at`, `clear_metadata`. `set_metadata(EntityMeta)` is **deleted** — the source-list type is an observation, not an instruction.

This implements **§4.3** (precedence when writing) and **§4.5** (per-key routing, nothing moves).

- [ ] **Step 1: Write the failing tests**

```rust
    /// §4.3 step 1: the thing already exists — write where it is.
    #[test]
    fn set_content_writes_where_content_already_lives() {
        // foo/e.md exists; the type declares layout = "inside".
        // set_content must rewrite foo/e.md, NOT create foo/e/content.md.
    }

    /// §4.3 steps 4-5: a node with nothing established follows intent.
    #[test]
    fn set_content_on_an_empty_node_follows_intended_layout() {
        // no files; type declares layout = "parallel" -> writes foo/e.md
    }

    /// §4.5: per-key routing. Nothing moves.
    #[test]
    fn updating_a_key_rewrites_the_source_that_already_holds_it() {
        // foo/e.md has a toml header with type = "T"; foo/e.meta.toml has word_count = 1.
        // set_meta_key("word_count", 2) rewrites the sidecar and leaves the header alone.
        // set_meta_key("type", "U") rewrites the header and leaves the sidecar alone.
        // Assert both files' full contents afterwards.
    }

    /// §4.5: a new key has no home, so it falls back to intent.
    #[test]
    fn a_new_key_goes_to_the_intended_sidecar() {
        // same tree, type declares layout = "parallel";
        // set_meta_key("added", true) lands in foo/e.meta.toml, not the header.
    }

    /// C7's third assertion.
    #[test]
    fn adding_metadata_to_a_mixed_node_does_not_normalise_it() {
        // foo/e.md + foo/e/meta.toml, type declares layout = "parallel".
        // set_meta_key("k", 1) writes foo/e/meta.toml (where metadata already lives),
        // and foo/e.meta.toml is never created.
    }

    #[test]
    fn removing_a_key_removes_it_from_wherever_it_lives() { /* ... */ }

    /// D3: a malformed source can be *replaced* but never *merged into*. Repair is the
    /// whole reason the raw text is retained, so it must have a way out.
    #[test]
    fn a_malformed_source_is_repairable_by_wholesale_replacement() {
        // foo/e.md + foo/e.meta.toml holding "x = = 1".
        // Merging refuses, naming the file and what the parser said — and nothing else.
        let err = live.set_meta_key("k", 1.into()).unwrap_err().to_string();
        assert!(err.contains("e.meta.toml"), "the refusal names the file: {}", err);
        // Replacing the whole source succeeds without the old text ever being parsed.
        live.set_metadata_at(MetaLocation::ParallelSidecar, m("k = 1")).unwrap();
        assert!(live.metadata().unwrap().malformed().is_empty());
        assert_eq!(live.metadata().unwrap().get_str_or_int("k").unwrap(), Some(1));
    }

    /// D3: and a caller who wants the bad file simply gone gets that too — the node
    /// afterwards reads as having no metadata, not as having unreadable metadata.
    #[test]
    fn clear_metadata_removes_a_malformed_source() {
        // same tree; clear_metadata() then metadata().is_none()
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib live_entity`
Expected: FAIL.

- [ ] **Step 3: Write the implementation**

Add a shared resolver, which is §4.3 expressed once:

```rust
impl LiveEntity {
    /// §4.3 for content: where a write should land.
    fn resolve_content_location(&self) -> anyhow::Result<ContentLocation> {
        if let Some(actual) = self.observed()?.content { return Ok(actual); }   // steps 1-2
        Ok(self.intended_layout()?.content_location())                          // steps 4-5
    }

    /// §4.3 for one metadata key.
    fn resolve_meta_location(&self, key: &str) -> anyhow::Result<MetaLocation> {
        if let Some(loc) = self.metadata()?.location_of(key) { return Ok(loc); } // step 1
        let obs = self.observed()?.metadata;
        if obs.len() == 1 { return Ok(obs[0]); }                                 // step 2
        Ok(self.intended_layout()?.sidecar_location())                           // steps 3-5
    }
}
```

`set_content(&self, text)` uses `resolve_content_location()`, preserving an existing in-header source by re-prepending it (as `:728-730` does today).

New per-key API:

```rust
    /// Sets one metadata key, routed per §4.5. Nothing else moves.
    pub fn set_meta_key(&self, key: &str, value: toml::Value) -> anyhow::Result<()>;
    /// Removes one metadata key from wherever it lives.
    pub fn remove_meta_key(&self, key: &str) -> anyhow::Result<()>;
    /// Replaces the whole of one source. Explicit, for callers that mean it.
    pub fn set_metadata_at(&self, location: MetaLocation, m: Metadata) -> anyhow::Result<()>;
    /// Removes every metadata source, including stripping an in-header block.
    pub fn clear_metadata(&self) -> anyhow::Result<()>;
```

`set_meta_key` reads the target source, inserts the key, and writes only that source back. When the target is `InHeader` it re-renders the header via `utils::format_metadata_header` and rewrites the content file, preserving the body. When the target sidecar does not exist yet it is created (§4.5: "if intent's location does not exist yet, it is created").

**Malformed sources: what refuses, and what must not.** Mirror Task 6's `EntityMeta::merged()` rule — a source that did not parse makes every *read-through* accessor fail (`get_str`, `get_vec_of_string`, `merged`), because answering "absent" would be a guess about a file the library could not read. The same reasoning makes `set_meta_key` and `remove_meta_key` return `Err`: both are read-modify-write, and the library will not merge into text it could not parse.

That is only defensible if the caller has a way out, so **three routes must stay open on a malformed node**, and each needs its test above:

1. **Reach it.** `metadata()` still returns the `EntityMeta`; `malformed()`, `source_at(loc)`, `raw()` and `error()` give the caller the offending text and the parse error verbatim. This is why D3 retains the raw text at all — without an accessor it would be dead weight.
2. **Replace it.** `set_metadata_at(location, m)` **overwrites the whole source** and therefore never parses what was there. It is the repair path and must not inherit the refusal. Its doc comment says so explicitly.
3. **Delete it.** `clear_metadata()` removes every source including malformed ones, leaving a node that reads as having no metadata rather than unreadable metadata.

The error from routes that do refuse states the problem and stops there: `"Metadata in {path} could not be parsed: {error}"`, via `MetaSource::describe()`. Two rules about that message, both of which cost real work to honour and are easy to get wrong:

- **It names a file, never a `MetaLocation` variant.** `"Metadata at ParallelSidecar could not be parsed"` is a Rust type name leaking into text that a person editing the tree will read, and it does not tell them which file to open. Task 3 was amended for this: `MetaSource` gained `entity: Option<EntityPath>`, set at every construction site in the loader and in `LiveEntity::metadata()`, and `describe()` **computes** the name — `ch1.meta.toml`, `ch1/meta.toml`, or `the front matter of ch1` — through `placement::sidecar_path(Path::new(""), entity, location)`.
- **It must not name `set_metadata_at` or any other API.** The reader of the message cannot act on advice to call a different Rust method. Routes 1–3 are documented where a programmer will actually look for them: the doc comments on the refusing methods.

**Store identity, not a path.** `(EntityPath, MetaLocation)` already determines the file, so a stored `PathBuf` would be derived state — duplicating `placement`, and going stale the moment `move_to` relocates the node. Storing the `EntityPath` also keeps the field in the same base-independent coordinate system as `Entity.path` and `EntityPath::local_path`, so `PartialEq` can stay derived: a tree written to a new base path and re-loaded compares equal to the original, which is what the round-trip tests assert. A `PathBuf` here would have made those two tests fail, and "exclude the field from equality" would have been the wrong fix for the wrong design.

`entity` is `Option` only because `ChildBuilder` holds metadata for a node that does not exist yet; `EntityMeta::unplaced(origin, m)` covers that and `describe()` falls back to kind-based wording. Task 10 replaces the builder's `EntityMeta` field with `(MetaLocation, Metadata)`, after which the `None` case has no producers and the field can lose its `Option`.

`delete` keeps its shape but derives its file list from `placement` rather than from `dot_metadata_path`'s substituting helper.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/live_entity.rs
git commit -m "feat: writes route to what a node has already established

Implements 4.3 precedence and 4.5 per-key metadata routing: nothing is
relocated as a side effect of writing an unrelated key."
```

---

## Task 10: `ChildBuilder` — a name, not an entry; no layout guessing

**Files:**
- Modify: `src/live_entity.rs` (`ChildContentLayout` `:44-51`, `ChildBuilder` `:57-69`, `with_content_*` `:87-109`, `with_child` `:146`, `build_internal` `:187-447`, `create_child` `:977`)
- Modify: `examples/create_child.rs`

**Interfaces:**
- Produces: `LiveEntity::create_child(&self, name: &str) -> ChildBuilder` (`#[must_use]`), `ChildBuilder::with_child(name, f)`, `::with_edge`, `::with_layout`, `::with_metadata_at`.
- **Deleted:** `ChildContentLayout`, `with_content_inside`, `with_content_parallel`, `with_metadata_inside`, `with_metadata_parallel`, and the `EntityPathEntry` parameter.

This is **C3** — the `Inferred` heuristic (slash⇒Inside, dot⇒Parallel) is exactly the coupling that makes §5's shape unwritable through the library.

- [ ] **Step 1: Write the failing test — this is the spec's required test case**

```rust
    /// §5: the shape the whole design exists to preserve. A readable spine file with a
    /// folder of children beside it, built entirely through the builder API.
    #[test]
    fn section_five_shape_is_buildable_through_the_api() {
        let mut schema = Schema::new();
        schema.add_entity_type(EntityTypeDescription {
            name: "Root".into(),
            children: vec![ChildEntityRules {
                name_regex: "^chapters$".into(), node_type: "Chapters".into(),
                required: false, multiple: false, edge: Edge::Slash,
            }],
            allow_additional: false, ignore: vec![], layout: None,
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapters".into(),
            children: vec![ChildEntityRules {
                name_regex: r"^\d{3}-".into(), node_type: "Chapter".into(),
                required: false, multiple: true, edge: Edge::Slash,
            }],
            allow_additional: false, ignore: vec![], layout: Some(Layout::Inside),
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Chapter".into(),
            children: vec![
                ChildEntityRules {
                    name_regex: r"^\d{3}-".into(), node_type: "Section".into(),
                    required: false, multiple: true, edge: Edge::Slash,
                },
                ChildEntityRules {
                    name_regex: "^review$".into(), node_type: "Section".into(),
                    required: false, multiple: false, edge: Edge::Slash,
                },
            ],
            allow_additional: false, ignore: vec![],
            layout: Some(Layout::Parallel),   // <- the spine file
        }).unwrap();
        schema.add_entity_type(EntityTypeDescription {
            name: "Section".into(), children: vec![], allow_additional: false,
            ignore: vec![], layout: Some(Layout::Parallel),
        }).unwrap();

        let mut raw = mockfs::MockFS::new();
        raw.create_dir_all(&PathBuf::from("book")).unwrap();
        raw.add_r(&PathBuf::from("book/meta.toml"), b"type = \"Root\"".to_vec()).unwrap();
        let fs = Arc::new(Mutex::new(raw));
        let root = LiveEntity::load_from_root_with(fs.clone(), PathBuf::from("book"), Arc::new(schema))
            .unwrap();

        let chapters = root.create_child("chapters").build().unwrap();
        let ch = chapters
            .create_child("000-the-invisible-kitchen")
            .with_content("Chapter body")
            .build()
            .unwrap();
        ch.create_child("010-what-fermentation-is")
            .with_content("Section body")
            .build()
            .unwrap();
        ch.create_child("review").with_content("Review body").build().unwrap();

        let fs = fs.lock().unwrap();
        let exists = |p: &str| fs.is_file(&PathBuf::from(p));
        // A readable spine file...
        assert!(exists("book/chapters/000-the-invisible-kitchen.md"));
        // ...with a folder of children beside it.
        assert!(exists("book/chapters/000-the-invisible-kitchen/010-what-fermentation-is.md"));
        assert!(exists("book/chapters/000-the-invisible-kitchen/review.md"));
        // And NOT the two shapes a layout/edge coupling would force.
        assert!(!exists("book/chapters/000-the-invisible-kitchen/content.md"));
        assert!(!exists("book/chapters/000-the-invisible-kitchen.010-what-fermentation-is.md"));
    }

    /// C4: a rule declaring edge = "slash" places a NEW child on the slash edge...
    #[test]
    fn new_child_follows_the_rules_declared_edge() { /* ... */ }

    /// ...and §4.2: but a new child under a rule whose existing children all sit on the
    /// other edge follows those siblings, not the declaration.
    #[test]
    fn new_child_follows_established_sibling_edge() { /* ... */ }

    /// §4.5: a split rule does not spread — new children conform even when siblings don't.
    #[test]
    fn new_child_under_a_split_rule_falls_back_to_the_declared_edge() { /* ... */ }

    /// §8.1: a child added to a hand-built node inherits that node's convention.
    #[test]
    fn child_of_a_hand_built_parallel_node_is_parallel() { /* ... */ }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib section_five_shape`
Expected: FAIL — `create_child` takes an `EntityPathEntry`.

- [ ] **Step 3: Write the implementation**

Delete the `ChildContentLayout` enum and the four location-forcing builder methods. `ChildBuilder` becomes:

```rust
#[derive(Debug, Clone)]
#[must_use = "a ChildBuilder does nothing until build() is called"]
pub struct ChildBuilder {
    root: Arc<LiveEntityRoot>,
    parent_path: EntityPath,
    parent_node_type: String,
    parent_inherited_layout: Layout,
    name: String,
    node_type_override: Option<String>,
    content_text: Option<String>,
    metadata: Option<(MetaLocation, Metadata)>,
    /// §8.1: explicit per-aspect overrides, the exception rather than the default path.
    edge_override: Option<Edge>,
    layout_override: Option<Layout>,
    nested_children: Vec<ChildBuilder>,
}
```

`create_child(&self, name: &str) -> ChildBuilder` — no `EntityPathEntry`. `with_child(name: &str, f)` likewise.

`build_internal` resolves in this order:

1. **Type** — `parent_ctype.match_child(&self.name)`, exactly as Task 5's resolver does. `Matched` supplies the type; `Additional` and an `Auto` slot require `with_type`, as today (`:213-245`).
2. **Edge**, per §4.3/§4.2:
   - `edge_override` if set;
   - else if a child of this name already exists, **that child's edge** — editing a child never moves it;
   - else the edges of existing children matching the *same rule index*: all on one edge → that edge; none → the rule's declared edge (or `Slash` for `Additional`); split → the rule's declared edge, since a split must not spread (§4.5).
3. **Layout**, per §4.3: `layout_override`, else the resolved child type's declared `layout`, else the **parent's intended layout** (§2.1 — inheritance is by instance, so this is `parent.intended_layout()`, not a schema lookup on the parent's type name).
4. **Root guard** — the root has no dot children (`:292-297`), now checked against the *resolved* edge.
5. **Existence checks** (`:302-346`) rewritten against the resolved edge and layout, using `placement` paths.
6. **Write** content at `placement::content_path(base, own_path, layout.content_location())`, and the sidecar at `placement::sidecar_path(base, own_path, metadata_location)` where `metadata_location` is the `with_metadata_at` override or `layout.sidecar_location()`. This replaces `:377-435`, in which the `Inferred` arm at `:380-383` is C3 itself and the `with_added_extension` at `:360`/`:413` is the one pair of sites that was already correct.
7. **Nested children** are built with `parent_inherited_layout = layout`.
8. Return a `LiveEntity` carrying `inherited_layout: layout`.

Add `with_edge(Edge)`, `with_layout(Layout)`, `with_metadata_at(MetaLocation, Metadata)`, and keep `with_type`, `with_content`, `with_metadata` (now taking `(MetaLocation, Metadata)`).

Add `LiveEntity::load_from_root_with(fs, root_path, schema)` for callers that supply a schema directly rather than reading `schema.toml`. It defaults the policy to `FindingPolicy::default()`; `load_from_root_with_policy(fs, root_path, schema, policy)` is the four-argument form.

Update `examples/create_child.rs` to the new API — it currently passes `EntityPathEntry::Slash(..)` / `::Dot(..)` and calls `with_metadata_inside`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test && cargo build --examples`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/live_entity.rs examples/create_child.rs
git commit -m "feat: create_child takes a name; layout and edge are resolved, not guessed

Deletes ChildContentLayout::Inferred (C3), the coupling that made the
section-5 shape unwritable through the library."
```

---

## Task 11: `move_to` — relocate, preserve layout, stop substituting

**Files:**
- Modify: `src/live_entity.rs` (`move_to` `:892-970`)

This implements **D5** and fixes the last C1 site (`:912`).

- [ ] **Step 1: Write the failing tests**

```rust
    /// C1's last site: a dot-child's sidecar must move to the appended name.
    #[test]
    fn move_relocates_a_dot_childs_appended_sidecar() {
        // foo/parent.notes.md + foo/parent.notes.meta.toml
        // move parent.notes -> parent.remarks
        // assert foo/parent.remarks.meta.toml exists and foo/parent.meta.toml is untouched.
    }

    /// §9.3: relocation drags descendants because their paths derive from this node's.
    #[test]
    fn move_drags_dot_descendants() {
        // foo/a.notes.draft.md moves with foo/a.notes
    }

    /// D5: layout survives; the resulting nonconformance is reported, not repaired.
    #[test]
    fn move_preserves_layout_and_reports_the_result() {
        // a parallel node moved under a parent whose type intends inside:
        // assert the files are still S.md / S.meta.toml at the new stem,
        // and that issues() on the moved node reports ContentLocationNonconformance.
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib move_`
Expected: FAIL — the sidecar lands at the substituted name.

- [ ] **Step 3: Write the implementation**

Keep the four-phase structure (dot content, dot sidecar, slash directory, prefix-matched dot descendants) and the `moved_anything` guard. Change:

- `:902` and `:912` both go through `placement::content_path` / `placement::sidecar_path` on the **new** path, so the sidecar appends.
- Move each of the node's own files by iterating `ContentLocation` and `MetaLocation` and renaming whichever exist, rather than assuming the dot pair — a node in `Inside` layout has `S/content.md` and `S/meta.toml` inside the directory, which the directory rename already carries, but a **mixed** node (C7) has one of each and today only the dot pair is handled.
- Update `self.path`, and recompute `self.inherited_layout` from the new parent if the caller supplies one; otherwise leave it, and document that a moved handle's inherited layout is stale until re-fetched via the new parent's `child()`.
- Add a doc comment stating plainly that `move_to` **relocates and does not normalise** (§9.3), and that normalisation is a separate operation arriving in the follow-up plan.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test`
Expected: PASS. `grep -rn 'with_extension' src/` returns nothing.

- [ ] **Step 5: Commit**

```bash
git add src/live_entity.rs
git commit -m "fix: move_to relocates without substituting the sidecar suffix

Completes C1. Layout survives a move (D5); the resulting nonconformance
is reported rather than silently repaired."
```

---

## Task 12: Docs, README, and the end-to-end round trip

**Files:**
- Modify: `README.md` (the storage-layout block around `:35` and the root-children line around `:63`)
- Replace: `docs/storage-layout.md`
- Modify: `docs/storage-layout-v2.md` (status header)
- Modify: `CHANGELOG.md`, `Cargo.toml` (version bump to `0.2.0`)

This is **C11**, plus the final integration test.

- [ ] **Step 1: Write the failing test**

```rust
    /// End-to-end: build the section-5 tree through LiveEntity, then read it back
    /// through EntityLoader and assert both agree with no findings.
    #[test]
    fn section_five_tree_round_trips_through_both_readers() {
        // 1. Build the tree exactly as section_five_shape_is_buildable_through_the_api.
        // 2. EntityLoader::try_load_root over the same MockFS.
        // 3. assert root.all_findings().is_empty()
        // 4. assert the chapter's content is Parallel, its layout is Layout::Parallel,
        //    and it has two Section children both on the slash edge.
        // 5. assert LiveEntity::child() reaches the same nodes with the same types.
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --lib round_trips_through_both_readers`
Expected: FAIL initially if any writer/reader disagreement remains.

- [ ] **Step 3: Update the docs**

`README.md` — C11. The line labelling `project.meta.toml` "Metadata for the root project (Parallel)" is wrong: the root is always `inside`, so root metadata is `meta.toml` **inside** the root directory, and a file named `project.meta.toml` there is a metadata-only **child** named `project`. Rewrite that line and the surrounding tree. Add a short section covering `layout` and `edge` with the §5 example, since that is the shape consumers most need.

`docs/storage-layout.md` — replace with a v2 as-built description: identity, layout, edge, discovery, tolerance rules, findings, and the write-routing precedence. It must describe the code as it now is, not as v2 aspires.

`docs/storage-layout-v2.md` — change the status header from "target design. Not implemented." to record that §1–§8 and §9.1 are implemented, that §9.2–9.4 and D7/D8 remain outstanding, and that D1–D6 are ruled as recorded in this plan's decisions table.

`CHANGELOG.md` — a `0.2.0` entry listing the breaking changes: `create_child` signature, `ChildContentLayout` removed, `EntityMeta` reshaped, `set_metadata` replaced, `add_entity_type` now fallible, `ignore` now regex.

Bump `Cargo.toml` to `0.2.0`.

- [ ] **Step 3a: Sweep the tests for stale doc references**

Every test written during this plan carries a doc comment naming the spec section or
decision it enforces (`§4.4`, `C1`, `D3`, ...). Those references drift silently: a section
can be renumbered, merged or rewritten while the test that cites it still passes, so
nothing catches it. Do the sweep once, here, after the docs above have settled.

**A bare `§4.4` does not say which document it is from.** This repo has at least three that
carry numbered sections — `docs/storage-layout-v2.md`, the v2 as-built `docs/storage-layout.md`
rewritten in Step 3 above, and this plan — and the v2 spec is the one that will be *retired*
once as-built lands, which is exactly when an unqualified reference becomes unresolvable.
Part of this sweep is therefore normalising every reference to name its file on first use in
each test module, e.g. a module-level `//! Section references are to docs/storage-layout-v2.md.`
with bare `§4.4` below it, or `storage-layout-v2 §4.4` inline where a module cites more than
one document. `C1`–`C11` and `D1`–`D6` are unambiguous today but belong to the v2 spec and this
plan respectively, so say so once per module too.

```bash
grep -rn '§[0-9]' src/ | sed 's/.*\(§[0-9.]*\).*/\1/' | sort -u
grep -rn '\bC1[01]\?\b\|\bC[2-9]\b\|\bD[1-8]\b' src/ --include='*.rs' | grep '///'
grep -rln '§' src/ | xargs grep -Ln 'Section references are to'   # modules missing the anchor
```

For each distinct reference, open the named document and confirm the section or defect still
exists, still has that number, and still says what the test claims it says. Where a section
number has moved, update the test comment. Where the claim itself has changed, the test is the
thing to fix, not the comment. Record any reference that no longer resolves to anything as a
finding for the follow-up plan rather than deleting the test.

- [ ] **Step 4: Run the full verification**

```bash
cargo test
cargo build --examples
cargo run --example create_child
grep -rn 'with_extension' src/          # must return nothing
grep -rn 'ChildContentLayout' src/      # must return nothing
grep -rn 'child_type' src/              # must return nothing
```

- [ ] **Step 5: Commit**

```bash
git add README.md docs CHANGELOG.md Cargo.toml src/live_entity.rs
git commit -m "docs: describe v2 layout and edge; fix root metadata line (C11)

Bumps to 0.2.0. Breaking: create_child takes a name, ChildContentLayout
and set_metadata are gone, EntityMeta is a source list, ignore is regex."
```

---

## Verification

**Per task:** each ends with `cargo test` green. The suite starts at 75 tests and should finish around 130.

**C1–C11 coverage** — the spec requires each defect to have a test that fails against today's behaviour and passes against v2:

| Defect | Test | Task |
| --- | --- | --- |
| C1 | `dot_child_sidecar_does_not_collide_with_parent`, `writing_a_dot_child_does_not_clobber_the_parents_sidecar`, `dot_child_parallel_sidecar_round_trips`, `move_relocates_a_dot_childs_appended_sidecar` | 1, 7, 11 |
| C2 | `duplicate_name_resolves_to_the_intended_edge`, `duplicate_name_with_no_rule_is_ambiguous_and_errors`, `child_and_children_agree_on_a_duplicated_name` | 5, 8 |
| C3 | `section_five_shape_is_buildable_through_the_api` | 10 |
| C4 | `rules_carry_an_edge_and_types_carry_a_layout`, `new_child_follows_the_rules_declared_edge`, `wrong_edge_still_loads_and_is_reported` | 2, 5, 10 |
| C5 | `invalid_regex_fails_at_schema_build`, `match_child_dispatches_...` | 2 |
| C6 | `ignore_entries_are_regexes` | 2 |
| C7 | `mixed_layout_loads_and_is_reported`, `adding_metadata_to_a_mixed_node_does_not_normalise_it` | 6, 9 |
| C8 | `matched_rule_with_no_files_is_not_an_error` | 5 |
| C9 | `required_and_multiple_are_reported_not_enforced` | 5 |
| C10 | `malformed_sidecar_is_reported_not_fatal`, `malformed_source_is_retained_alongside_a_good_one` | 3, 6 |
| C11 | README review during Task 12 | 12 |

**End-to-end:** `section_five_tree_round_trips_through_both_readers` builds §5's tree through the public builder API, reads it back through `EntityLoader`, and asserts zero findings — which is the single strongest check that the writer and reader now agree.

**Manual:** `cargo run --example create_child` and read its printed tree against §5.

**Downstream (not in this plan):** `inscenerator-booker-agents4/crates/booker-core` consumes this crate through a workspace dependency and will not compile against `0.2.0`. The other consumers (`inscenerator-booker`, `-agents`, `-agents2`, `-agents3` on `0.1.6`; `inscenerator-rs` on `0.1.1`) are pinned to crates.io releases and are unaffected until they bump. Migrating `agents4` — including replacing the raw Lua path arithmetic in `generate-section-outline.lua:32` with a real `add_child` call, and §8.2's table-call scripting surface — is a separate piece of work in that repository.

## Follow-up plan (out of scope here)

§9.2–9.4: `NormaliseSpec`, `plan_normalise`, `normalise`, `normalise_to_schema`, `MovePlan`/`MoveReport`, collision validation, metadata merging on repair, and rulings for D7 (what happens to a stray during normalisation) and D8 (surviving an I/O failure mid-execution). It depends on §4's observation machinery, which this plan delivers.
