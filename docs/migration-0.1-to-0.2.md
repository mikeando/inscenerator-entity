# Migrating from 0.1 to 0.2

0.2 makes the **schema** say where things go. In 0.1 a caller had to know the on-disk convention
to create a child; now the parent's rule declares the child's **edge** and the child's type
declares its **layout**, and `create_child` takes a name.

`docs/storage-layout.md` describes the result. This page covers what changed.

---

## Layout

### Nothing on disk has to move

0.1's two shapes are 0.2's two layouts, unchanged:

| | Content | Metadata |
| --- | --- | --- |
| "inside" / directory | `S/content.md` | `S/meta.toml` |
| "parallel" / file | `S.md` | `S.meta.toml` |

An existing tree loads as-is. Where it disagrees with the schema you now write down, it still
loads, and the difference is reported as a finding rather than raised as an error.

### Dot-children's sidecars move

The one on-disk change. In 0.1 a dot-child's parallel sidecar path was computed by *substituting*
the extension, so the child `a/b.review` resolved to `a/b.meta.toml` — its **parent's** file.
Writing the child overwrote the parent's metadata, and the eager loader could not read back what
`LiveEntity` had written.

0.2 appends: `a/b.review` has the sidecar `a/b.review.meta.toml`.

If a 0.1 tree has dot-children with parallel sidecars, that metadata was being written to the
parent's file and is already lost or merged. There is nothing to migrate — but check any
`*.meta.toml` beside a node with dot-children for keys that belong to a child.

### Shapes that used to be errors

Three trees that 0.1 refused now load:

- `S.md` **and** `S/content.md` — the intended layout picks, the other is a `StrayContent` finding.
- A node with content in one layout and its sidecar in the other — it loads, stays mixed, and is
  reported. Writing an unrelated key does not normalise it.
- Metadata in a header **and** a sidecar — they merge per key. Only the same key with two
  *different* values is an error.

A metadata file that does not parse no longer aborts the load either: the source is kept with its
raw text and the parse error, and written back verbatim.

### Two new schema fields

```toml
[Chapter]
allow_additional = false
layout = "parallel"          # NEW: where this type's own files live
ignore = ['^\.']             # CHANGED: regexes now, was exact strings

[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"               # NEW: how children attach; defaults to "slash"
```

Both are optional. A type omitting `layout` inherits it from the parent *instance* it was reached
under. A rule omitting `edge` gets `slash`.

**`ignore` is the one field whose meaning changed.** `ignore = ["booker-data"]` was an exact name
match and is now a regex — as written it still matches `booker-data`, but it also matches
`my-booker-data-old`. Anchor entries you want exact: `ignore = ['^booker-data$']`.

---

## API

### Creating children

```rust
// 0.1
parent.create_child(EntityPathEntry::Slash("010-intro".into()))
    .with_content_parallel("body")
    .with_metadata_inside(meta)
    .build()?;

// 0.2
parent.create_child("010-intro")
    .with_content("body")
    .with_metadata(meta)
    .build()?;
```

`ChildContentLayout` is gone, along with `with_content_inside` / `with_content_parallel` /
`with_metadata_inside` / `with_metadata_parallel`. The schema decides. Where you genuinely need to
override, `with_edge(Edge)`, `with_layout(Layout)` and `with_metadata_at(MetaLocation, Metadata)`
do it one aspect at a time.

`with_child(name, |b| ...)` takes a name too, and `ChildBuilder::with_metadata` takes a
`Metadata` where 0.1's took an `EntityMeta` — the location is no longer the caller's to pick.

### Metadata

`EntityMeta` was an enum of one source; it is now the list of sources observed on disk.

```rust
// 0.1
match entity.metadata {
    EntityMeta::Parallel(m) => ...,
    EntityMeta::InHeader(m, sep, ht) => ...,
    ...
}

// 0.2
let merged = entity.metadata.merged()?;              // Option<Metadata>, all sources folded
let value  = entity.metadata.get_str("title")?;      // as before
let where_ = entity.metadata.location_of("title");   // Option<MetaLocation>
for s in entity.metadata.sources() { ... }           // each source, with its state
```

Constructing one is `EntityMeta::of(sources)`, or `::parallel(path, m)` / `::inside(path, m)` /
`::in_header(...)` for the single-source cases. Each source carries its `EntityPath`, so its file
is derived from `placement` rather than stored.

`LiveEntity::set_metadata(EntityMeta)` is gone — a source list describes what is there, and is not
a sensible instruction. Replacements:

| | |
| --- | --- |
| `set_meta_key(key, value)` | sets one key, in whatever source already holds it |
| `remove_meta_key(key)` | removes one key from wherever it lives |
| `set_metadata_at(location, m)` | replaces one whole source, explicitly |
| `clear_metadata()` | removes every source, header included |

### Schema

`Schema::add_entity_type` returns `anyhow::Result<()>`: it compiles the type's regexes, so an
invalid pattern fails there instead of at some later lookup. Add `?` at each call site.

`EntityTypeDescription::child_type` is deleted. `Schema::compiled(name)?.match_child(name)` is the
one rule matcher, returning `Ignored` / `Matched` / `Additional` / `Unexpected`.

### Loading

`EntityLoader::try_load_entity` takes an `inherited_layout`. Use `try_load_root` instead:

```rust
// 0.2
let root = loader.try_load_root(&fs, &base, "Project")?;
for f in root.iter().flat_map(|e| e.all_findings()) { eprintln!("{}", f.kind); }
```

`Entity` gains `layout` (what it was resolved under) and `findings` (drift seen on that node);
`Entity::all_findings()` walks the subtree. On the lazy side, `LiveEntity::issues()` computes the
same for one node, and never fails on a finding.

### Findings and severity

New in 0.2, and the reason reads are tolerant. Each `FindingKind` has a `Severity` under a
`FindingPolicy`: `Ignore`, `Warn`, or `Error` (which fails the call that produced it).
Nonconformance defaults to `Warn`; ambiguity with no defined resolution defaults to `Error`.

```rust
let loader = EntityLoader::new().with_policy(FindingPolicy::strict());   // CI
let root   = live_root.with_policy(FindingPolicy::silent());             // data only
```

`FindingPolicy::default().with(FindingKindId::StrayContent, Severity::Error)` overrides one kind.

To keep 0.1's behaviour of failing on a tree that disagrees with itself, use
`FindingPolicy::strict()`. To keep loading without being told about drift, use `silent()`.

### Moving

`move_to` relocates and does not normalise: every file keeps the layout it arrived with, at the
new stem, and dot-descendants are dragged along. If the new position intends a different layout,
that shows up in `issues()` on the moved node. Re-fetch the handle through the new parent's
`child()` — a moved handle's inherited layout is stale if it changed parents.

---

## Checklist

1. Add `?` to every `add_entity_type` call.
2. Anchor any `ignore` entry that meant an exact name: `["x"]` → `['^x$']`.
3. Declare `layout` on the types that need it, and `edge = "dot"` on the rules that need it.
   Everything else defaults to what 0.1 did most of the time.
4. Replace `create_child(EntityPathEntry::…)` with `create_child(name)`, and drop the
   `with_content_*` / `with_metadata_*` variants.
5. Replace `EntityMeta` matches with `merged()` / `get_str()` / `sources()`.
6. Replace `set_metadata` with `set_meta_key` or `set_metadata_at`.
7. Replace `try_load_entity` with `try_load_root`.
8. Decide a `FindingPolicy`, and read `all_findings()` or `issues()` somewhere.
