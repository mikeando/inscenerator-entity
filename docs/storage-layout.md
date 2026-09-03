# Storage Layout — As-Built Description

> **Superseded by [`storage-layout-v2.md`](storage-layout-v2.md).**
>
> This file describes what the code does **today**. It is retained as the reference for current
> behaviour and as the source of the defect catalogue — the open questions below are carried
> forward as C1–C11 and D1–D4 in v2, where each is paired with what the target design must do
> instead.
>
> Do not fix the defects listed here in isolation. The v2 design changes the surrounding model
> enough that isolated fixes would be rework.

How an entity finds its **content**, its **metadata**, and its **children** on disk.

This spec is written against the behaviour of `src/entity.rs` as of commit `1cb17c4`. It is
descriptive where the code is believed correct and **explicitly flags where it is not** — see
[Open questions](#open-questions).

The variety of layouts is deliberate. A note attached to a node should be able to be
`my_node.my_note.md` with inline TOML — one file — rather than forcing
`my_node/my_note/content.md` plus `my_node/my_note/meta.toml`. The rules below exist to make that
choice free.

---

## 1. Logical paths and disk paths

An entity is identified by an `EntityPath`: an ordered list of entries, each either
`Slash(name)` or `Dot(name)`. The **root** has an empty entry list.

`disk(P)`, the filesystem path an entity path denotes, is built by folding the entries over the
base path (`EntityPathEntry::to_pathbuf`, entity.rs:78-87):

| Entry | Effect on the accumulated path |
| --- | --- |
| `Slash(name)` | `acc.join(name)` |
| `Dot(name)` | `acc.with_file_name(format!("{filename_of_acc}.{name}"))` |

```
[Slash("chapters"), Slash("ch1")]              ->  chapters/ch1
[Slash("chapters"), Slash("ch1"), Dot("notes")] ->  chapters/ch1.notes
[Slash("chapters"), Slash("ch1"), Dot("notes"), Dot("draft")]
                                                ->  chapters/ch1.notes.draft
```

`disk(P)` never carries a file extension. It is a **stem**: the rules below derive concrete
filenames from it by appending suffixes or descending into it as a directory.

**Constraint: an entity name may not contain a `.`.** Both child-discovery rules split on the
first dot (§4), so a file `my.file.md` is read as the entity `my` with a dot-child `file`, never as
an entity named `my.file`. This is a consequence of the dot-child design, not a bug.

---

## 2. Content

Two locations, checked in this order (entity.rs:479-497):

| Layout | File | Enum |
| --- | --- | --- |
| **Parallel** | `disk(P)` + `.md` — *suffix appended* | `EntityContent::Parallel` |
| **Inside** | `disk(P)/content.md` | `EntityContent::Inside` |

- Parallel is tried first. If it exists **and** the Inside file also exists, that is an
  **error**: `"Both {dot} and {slash} exist."` (entity.rs:485).
- If neither exists, the entity has `EntityContent::None`. This is legal — see §5.
- **The root is Inside-only.** A root has no filename to append to, so `disk(root)/content.md` is
  the only content location considered (entity.rs:495). The root must also *be* a directory: if
  `disk(root)` is not one, loading fails outright with
  `"Root entity at {path} must be a directory"` (entity.rs:474).

Note the suffix is **appended**, not substituted: for the dot-child `a/b.review`, content is at
`a/b.review.md`. Substituting the extension would yield `a/b.md` — the parent's file.

---

## 3. Metadata

Three sources. They are **mutually exclusive**, not ranked: supplying more than one is an
**error**, `"Multiple metadata sources found for entity at {path}."` (entity.rs:538).

| Source | Location | Enum |
| --- | --- | --- |
| **Parallel sidecar** | `disk(P)` + `.meta.toml` — *see [Q1](#q1)* | `EntityMeta::Parallel` |
| **Inside sidecar** | `disk(P)/meta.toml` | `EntityMeta::Inside` |
| **In-header** | front matter inside the content file | `EntityMeta::InHeader` |

- **The root has no Parallel sidecar** (entity.rs:517-521 gates it on `!is_root`), for the same
  reason it has no Parallel content. Root metadata is `disk(root)/meta.toml` or in-header in
  `disk(root)/content.md`.
- In-header requires content, by construction — it is parsed *out of* the content file. On the
  write side this is enforced: `EntityWriter` errors with `"Metadata from header requires content"`
  (entity.rs:683).
- Metadata layout is **independent of content layout**. Parallel content with an Inside sidecar
  (`e.md` + `e/meta.toml`) is legal, as is Inside content with a Parallel sidecar
  (`e/content.md` + `e.meta.toml`). See [Q4](#q4).
- A sidecar is parsed as **TOML only** (`toml::from_str`, entity.rs:278). A malformed sidecar is a
  hard error that aborts the whole load — it is not reported as a per-entity problem and there is
  no recovery. In-header metadata behaves differently: a header that fails to parse is treated as
  *absent*, and the text becomes body (§3.1). See [Q7](#q7).

### 3.1 In-header front matter

`parse_header` (entity.rs:385) tries YAML first, then TOML.

**YAML** (`HeaderType::Yaml`) — the file must *begin* with a `---` line; the block ends at the next
`---` line. No leading blank lines are permitted.

```markdown
---
id: ch1
type: chapter
---

Body text.
```

**TOML** (`HeaderType::Toml`) — leading blank lines are skipped, then a line opening with ```` ``` ````
whose remainder is empty or exactly `toml`; the block ends at the next line opening with ```` ``` ````
and nothing else.

````markdown
```toml
id = "ch1"
type = "chapter"
```

Body text.
````

After a TOML fence, an optional **thematic break** — a line of three or more `-`, `_` or `*`,
indented at most three spaces — is captured as a *separator* and preserved for round-tripping
(entity.rs:359-377). Only blank lines may precede it.

````markdown
```toml
id = "ch1"
```

---

Body text.
````

Here the separator is `"\n---\n"` and the body is `"\nBody text.\n"`. A file that fails both
parsers simply has no in-header metadata; its whole text is the body.

---

## 4. Children

Children come from two independent discovery passes. Each pass sorts and deduplicates **within
itself** (each builds its own `BTreeSet`), and the results are then concatenated dot-pass first
(entity.rs:551-554):

```rust
let children = dot_children.into_iter().chain(slash_children).collect::<Vec<EntityPath>>();
```

Two consequences:

- **The combined list is not globally sorted** — it is dot children in name order, then slash
  children in name order.
- **There is no cross-pass deduplication.** If both `ch1.notes.md` and `ch1/notes.md` exist, the
  entity `ch1` gets **two** children, both named `notes`, dispatched independently against the
  same rules. See [Q6](#q6).

### 4.1 Dot children — `find_dot_children` (entity.rs:182)

Scans the **parent directory** of `disk(P)` for entries whose name begins with
`{filename(disk(P))}.`:

1. Strip that prefix, leaving a suffix.
2. Skip the suffix if it is exactly `md` or `meta.toml` — those are the entity's *own* files.
3. Otherwise the child's name is the suffix up to its first `.`.

```
entity: chapters/ch1
  chapters/ch1.md              -> suffix "md"              -> skipped (own content)
  chapters/ch1.meta.toml       -> suffix "meta.toml"       -> skipped (own metadata)
  chapters/ch1.notes.md        -> suffix "notes.md"        -> child "notes"
  chapters/ch1.notes.meta.toml -> suffix "notes.meta.toml" -> child "notes"
  chapters/ch1.notes.draft.md  -> suffix "notes.draft.md"  -> child "notes"
```

The last line is how nesting works: `ch1.notes.draft.md` registers `notes` as a child of `ch1`.
When `ch1.notes` is loaded in turn, *its* dot-scan finds `draft`. `ch1.notes` needs no file of its
own to exist — having children is sufficient (§5).

**The root has no dot children** (entity.rs:547) — it has no filename to prefix with.

### 4.2 Slash children — `find_slash_children` (entity.rs:232)

If `disk(P)` is a directory, scans its entries. Skips exactly `content.md`, `meta.toml` and
`schema.toml`. The child's name is each remaining entry's **file prefix** — the portion before its
first `.`.

```
entity: chapters/ch1   (directory chapters/ch1/ exists)
  chapters/ch1/content.md    -> skipped (own content)
  chapters/ch1/meta.toml     -> skipped (own metadata)
  chapters/ch1/schema.toml   -> skipped (schema, not an entity)
  chapters/ch1/s1.md         -> child "s1"
  chapters/ch1/s1.meta.toml  -> child "s1"      (deduplicated with the line above)
  chapters/ch1/s2/           -> child "s2"
```

Taking the file prefix is what unifies the layouts: `s1.md`, `s1.meta.toml` and a directory `s1/`
all name the same child `s1`, which then resolves its own content and metadata by §2 and §3.

Note the skip list applies to **this pass only**. `schema.toml` is skipped as a slash child, but a
file named `S.schema.toml` beside the entity would be picked up by the dot pass (§4.1) as a child
named `schema`, since that pass skips only the suffixes `md` and `meta.toml`.

### 4.3 Dispatch to types — entity.rs:596-626

For each discovered child name, in sorted order:

1. If the name matches the parent type's `ignore` list, skip it. **`ignore` is exact-string
   match**, unlike `children`, which is regex — see [Q3](#q3).
2. Otherwise walk the parent type's `children` rules **in declaration order**. On the first rule
   whose `name_regex` matches, recurse with that rule's `node_type`.
   - If the recursion yields an entity, record it and stop scanning rules.
   - If it yields `None` (the child does not exist — §5), **continue to the next matching rule**.
3. If no rule produced an entity and the parent type has `allow_additional = false`, that is an
   **error**: `"Unexpected child entity '{name}' in entity '{path}'"`.

Consequence worth stating plainly: **a name that matches a rule but has no files behind it still
errors.** A `.gitkeep` placeholder cannot be handled by adding a rule for it; it must go in
`ignore`, because the rule match yields `Ok(None)` and step 3 then rejects it.

`required` and `multiple` on a rule are **parsed and never enforced** — no non-test code reads
them. See [Q5](#q5).

---

## 5. When does an entity exist?

`try_load_entity` returns `Ok(None)` — meaning "no such entity" — only when **all four** of these
hold (entity.rs:556-559):

- no content file, and
- no metadata from any source, and
- no children, and
- no directory at `disk(P)`.

Equivalently: an entity **exists** if it has *any* of content, metadata, children, or a directory.

Three cases follow that consumers commonly get wrong:

- **A metadata-only entity is real.** `e.meta.toml` with no `e.md` and no `e/` is a loadable
  entity with `EntityContent::None`.
- **An empty directory is a real entity.** `e/` with nothing in it loads, with no content and no
  metadata. This is how container directories (`chapters/`, `research/`) are modelled.
- **An intermediate dot-entity is real.** `ch1.notes.draft.md` alone makes `ch1.notes` exist,
  carrying nothing but the child `draft`.

---

## 6. Type resolution — entity.rs:561-591

The type a child is loaded as comes from the **parent's rule**, not from the child's own metadata.

- If the requested type is the literal `"Auto"`, the type is read from the entity's metadata `type`
  key. Absent metadata, or a missing `type` key, is an error. A metadata `type` of `"Auto"` is
  rejected.
- Otherwise the requested type is used, and if the entity's metadata carries a `type` key that
  **differs**, that is an error:
  `"Entity at '{path}' has type '{found}' in metadata, but was expected to be '{expected}'"`.

So `type` in metadata is a redundant, checked assertion — except under `Auto`, where it is the sole
source.

---

## 7. Summary tables

### Files an entity at stem `S` may own

| File | Role | Root? |
| --- | --- | --- |
| `S.md` | Parallel content | ✗ |
| `S.meta.toml` | Parallel metadata sidecar | ✗ |
| `S/content.md` | Inside content | ✓ |
| `S/meta.toml` | Inside metadata sidecar | ✓ |
| `S/schema.toml` | schema — never an entity | ✓ |
| `S.<name>…` | dot children | ✗ |
| `S/<name>` | slash children | ✓ |

### Legal content × metadata combinations

| | no metadata | in-header | `S.meta.toml` | `S/meta.toml` |
| --- | --- | --- | --- | --- |
| **no content** | ✓ *(needs children or a directory)* | — *impossible* | ✓ | ✓ |
| **`S.md`** | ✓ | ✓ | ✓ | ✓ *(see Q4)* |
| **`S/content.md`** | ✓ | ✓ | ✓ *(see Q4)* | ✓ |

Any two metadata cells at once → error. `S.md` and `S/content.md` at once → error.

---

## Open questions

These need a ruling before tests are written against them. Each is a place where the code, the
README, and the library's own test suite do not agree; **none is resolved here.**

### Q1 — Is a dot-child's metadata sidecar appended or substituted?

`src/entity.rs` derives the two sibling files by different rules, four lines apart:

```rust
// entity.rs:480  content  — APPENDS:    a/b.review -> a/b.review.md
let dot_content_file  = entity_path.to_pathbuf(base_path).with_added_extension("md");
// entity.rs:516  metadata — SUBSTITUTES: a/b.review -> a/b.meta.toml
let dot_metadata_file = entity_path.to_pathbuf(base_path).with_extension("meta.toml");
```

Evidence that **appended** is intended:

1. `README.md:19` gives Parallel metadata as `P.meta.toml`; for `P = a/b.review` that is appended.
2. Under substitution the dot-child `a/b.review` and its parent `a/b` resolve to the **same**
   metadata file — one file supplying both entities' metadata wholesale, including the parent's
   `type`, which then fails the child's own type check at entity.rs:585.
3. `src/live_entity.rs:1726`, `test_with_metadata_parallel_dot_child`, writes a dot-child's
   parallel metadata and asserts it lands at `foo/parent.notes.meta.toml`. It passes today. So
   `EntityLoader` cannot read back what `LiveEntity` writes.

Three sites use the substituting form: `entity.rs:516` (reader), `entity.rs:693`
(`EntityWriter::write_entity` — and children are written *after* the parent, so a dot-child with
parallel metadata **overwrites its parent's file**), and `live_entity.rs:912` (rename). Two sites
use the appending form: `live_entity.rs:360` and `:1000`.

**Ruling needed:** confirm appended, and that all three substituting sites are the defect.

### Q2 — Is `README.md:35` wrong about root metadata?

The README's example layout labels `project.meta.toml` as "Metadata for the root project
(Parallel)". But the Parallel sidecar is gated on `!is_root` (entity.rs:517), and README:63 itself
says the root "can only have Slash type children". Under the code, root metadata is
`my-project/meta.toml`, and a file literally named `project.meta.toml` inside the root would be
discovered by `find_slash_children` as a *metadata-only child entity* named `project`.

**Ruling needed:** confirm the README line is wrong and the code is right.

### Q3 — Should `ignore` accept regexes?

`ignore` is exact-string match (entity.rs:600) while `children` is regex. Real trees carry
`.gitkeep` in every scaffolded directory, plus `README.md`, `.gitignore`, and tool directories, so
every type's `ignore` list repeats the same literals. Making `ignore` a regex list mirrors
`children` — but it silently changes the meaning of existing entries (`".config"` becomes an
unanchored pattern).

**Ruling needed:** regex, or keep exact-match?

### Q4 — Are mixed content/metadata layouts intended?

The code permits `e.md` + `e/meta.toml`, and `e/content.md` + `e.meta.toml`. Neither is documented.
The first arises naturally: a node with Parallel content whose directory holds its slash children,
where someone also drops a `meta.toml` in.

**Ruling needed:** document as supported, or make it an error?

### Q5 — Should `required` and `multiple` be enforced?

Both are declared on `ChildEntityRules` (schema.rs:16-19), parsed, and read by no non-test code. A
schema author writing `required = true` today gets nothing.

**Ruling needed:** enforce them (and as errors, or as reported violations?), or delete the fields?

### Q6 — May the same child name arrive from both discovery passes?

The two passes are concatenated without cross-pass deduplication (§4). If `ch1.notes.md` and
`ch1/notes.md` both exist, `ch1` gets two distinct children both named `notes` — one `Dot`, one
`Slash` — each dispatched against the same rules and each loaded independently. Nothing detects
or reports the collision. A consumer keyed by child name (booker's node ids, for instance) would
see a duplicate.

**Ruling needed:** is this an error, a reported violation, or legitimately two entities that merely
share a name?

### Q7 — Should a malformed sidecar be fatal when a malformed header is not?

A `.meta.toml` that fails to parse aborts the entire load (entity.rs:278 propagates the
`toml::from_str` error). A content file whose front matter fails to parse is silently treated as
having no metadata, with the unparsed text becoming the body (§3.1). The same authoring mistake
therefore either stops everything or passes unnoticed, depending only on where the author put the
metadata.

**Ruling needed:** should these agree, and in which direction?
