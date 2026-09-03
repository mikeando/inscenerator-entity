# Storage Layout v2 — Target Spec

**Status: §1–§8 and §9.1 are implemented as of 0.2.0. §9.2–§9.4 (normalisation, move planning)
remain outstanding, as do rulings for D7 and D8. D1–D6 are ruled as recorded below.
[`storage-layout.md`](storage-layout.md) is the as-built description of the implemented code;
this document remains the design argument behind it.**

The defect catalogue C1–C11 is closed: each defect has a test that fails against the old
behaviour and passes now. §8.2, the Lua `add_child{}` table-call surface, lives in
`inscenerator-booker-agents4` and was never in scope here.

**Backwards compatibility is explicitly not a goal.** Existing trees will need migrating or
regenerating. Existing API shapes may be deleted outright rather than deprecated. Where this spec
and the current code disagree, this spec wins and the code is wrong.

---

## 0. The problem this solves

Today, three things are decided independently and none of them is written down anywhere the
library can consult:

- where an entity's **content** lives (`S.md` or `S/content.md`),
- where its **metadata sidecar** lives (`S.meta.toml` or `S/meta.toml`),
- whether a child hangs off the **dot edge** (`S.name`) or the **slash edge** (`S/name`).

The reader infers all three from what happens to be on disk. The writer guesses two of them from a
heuristic (`ChildContentLayout::Inferred`: slash entries get Inside, dot entries get Parallel). The
schema knows none of them — `ChildEntityRules` has no edge field, and `child_type` takes a bare
`&str` with the edge already discarded.

The consequence is that **a caller cannot create a child without already knowing the on-disk
convention**, which is why consumers end up doing raw path arithmetic instead of asking the
library. That is the thing to fix.

The fix is two declarations in the schema:

> **The parent's rule says which edge a child attaches on. The child's type says where its content
> and metadata live.**

Those are different declarations in different places, and they are orthogonal.

The schema declares an **intent**, not a guarantee. A tree that humans edit by hand will contain
nodes that do not match it, and those nodes must still load, and must still be extendable on their
own terms. So every placement question is answered twice — once by the schema and once by what is
actually on disk — and §4 says which one wins, for each aspect, in each situation. Read operations
tolerate the difference; write operations follow what a node has already established.

---

## 1. Identity — unchanged

An entity is identified by an `EntityPath`: an ordered list of entries, each `Slash(name)` or
`Dot(name)`. The root has an empty list.

`disk(P)` folds the entries over the base path:

| Entry | Effect |
| --- | --- |
| `Slash(name)` | `acc.join(name)` |
| `Dot(name)` | `acc.with_file_name(format!("{filename_of_acc}.{name}"))` |

```
[Slash("chapters"), Slash("ch1")]                          ->  chapters/ch1
[Slash("chapters"), Slash("ch1"), Dot("notes")]            ->  chapters/ch1.notes
[Slash("chapters"), Slash("ch1"), Dot("notes"), Dot("x")]  ->  chapters/ch1.notes.x
```

`disk(P)` never carries an extension. It is a **stem**.

**Constraint retained: an entity name may not contain a `.`.** A file `my.file.md` is the entity
`my` with a dot-child `file`, never an entity named `my.file`. This is inherent to the dot-child
design.

---

## 2. Layout — declared on the entity type

```toml
[entity_types.chapter]
layout = "parallel"
```

| Layout | Content | Metadata sidecar |
| --- | --- | --- |
| `parallel` | `S.md` | `S.meta.toml` |
| `inside` | `S/content.md` | `S/meta.toml` |

Rules:

- **Both suffixes are appended to the stem, never substituted.** For the dot-child
  `a/b.review` under `parallel`, content is `a/b.review.md` and the sidecar is
  `a/b.review.meta.toml`. Substituting would yield `a/b.md` and `a/b.meta.toml` — the *parent's*
  files. See [C1](#c1).
- **Content and sidecar always agree — as an intent.** A type declares one layout, which picks
  both. `S.md` + `S/meta.toml` is not something you can *declare*. It is still something you can
  *read*: see §4, and [C7](#c7). This deletes an 8-cell matrix from what a schema can express,
  not from what exists on disk.
- **The root is always `inside`.** It has no filename to append to. This is enforced, not
  declared; a root type declaring `parallel` is a schema error. The root must also *be* a
  directory.
- **Layout does not constrain children.** A `parallel` entity may still have a directory full of
  slash children. This is the shape the whole design exists to preserve — see §5.

### 2.1 Default layout

When a type omits `layout`, it **inherits the layout of the parent entity it was loaded under**.
The root defaults to `inside`.

Inheritance is by *instance*, not by type: the same type reached under two different parents
inherits differently. A type that wants to be stable across parents should declare its layout
explicitly. Declaring is recommended for every type; inheritance exists so that `Auto` and
`allow_additional` children have a defined answer (§7.2).

---

## 3. Edge — declared on the child rule

```toml
[[entity_types.chapter.children]]
name_regex = '^\d{3}-'
node_type  = "section"
edge       = "slash"
```

| Edge | Child stem |
| --- | --- |
| `slash` | `disk(parent)/name` |
| `dot` | `disk(parent).name` |

Rules:

- **`edge` defaults to `slash`.** Dot is the compact case you opt into deliberately.
- **A child found on the wrong edge still loads.** If a rule declares `edge = "slash"` and
  `S.name.md` exists on disk, that child is read normally and its nonconformance is reported
  (§4, §9). It is not an error and not a second valid child.
- **The root has no dot children** — it has no filename to prefix with. A root type whose rules
  declare `edge = "dot"` is a schema error.
- Edge is on the **rule**, not the type, because the same type legitimately attaches differently
  under different parents — a `note` may be a dot-child of a section and a slash-child of a
  chapter.

### 3.1 Discovery

Discovery still runs two passes, because it must read what is on disk before it can judge it. The
change is that the passes' results are now **checked against the declared edge** rather than
concatenated blindly.

- **Dot pass** — scan `disk(P)`'s parent directory for entries beginning `{filename(disk(P))}.`;
  strip the prefix; skip the suffixes `md` and `meta.toml` (the entity's own files); the child's
  name is the remainder up to its first `.`.
- **Slash pass** — if `disk(P)` is a directory, scan it; skip exactly `content.md`, `meta.toml`,
  `schema.toml`; the child's name is each entry's prefix up to its first `.`.

Both passes produce names. The union is then deduplicated **across passes** and each name resolved
once — never two children sharing a name. Where a name arrives from both passes, the rule's
intended edge decides which is the child and which is a reported stray (§4.4). See [C2](#c2).

Nesting still works as before: `ch1.notes.draft.md` registers `notes` as a child of `ch1`, and
`ch1.notes` needs no file of its own (§6).

---

## 4. Intended layout vs actual layout

§2 and §3 describe what a type and a rule **intend**. What is on disk may differ — a hand-created
node, a file a human moved between agent runs, a tree built before the schema changed. Both facts
matter, and they answer different questions:

- **Intended** layout and edge come from the schema. They answer *where does a new thing go?*
- **Actual** layout and edge are observed from disk. They answer *where do existing things already
  live?*

Two rules follow, and they are the whole point of the distinction:

**Reading is tolerant.** A node whose files are otherwise fine but do not match its type's intended
layout **loads normally**. Nonconformance is recorded and reportable (§9), never an error in
itself. This is not leniency for its own sake: humans edit these trees by hand between agent runs,
and a tree that refuses to open because someone moved a file is a worse failure than one that opens
and says so.

**Writing follows what is already there.** Adding content, metadata, or a child to an existing node
uses that node's *actual* convention, falling back to intent only where the node has not
established one. A node keeping its metadata in a sidecar keeps getting sidecar metadata, even if
its type declares in-header.

### 4.1 What is observed

Actual layout has three independent aspects, and each resolves to one of **three** outcomes, not
two:

- **`None`** — nothing on disk establishes it. Defer to intent.
- **one value** — a convention is established. Follow it.
- **several values** — the node is ambiguous about this aspect. See §4.5.

| Aspect | Observed from | Values |
| --- | --- | --- |
| **Content location** | which content file exists | `Parallel` (`S.md`), `Inside` (`S/content.md`) |
| **Metadata location** | which metadata sources exist | `ParallelSidecar`, `InsideSidecar`, `InHeader` |
| **Child edge** | which edge existing children sit on | `Slash`, `Dot` — *per rule*, see §4.2 |

Ambiguity is not exotic. A node with `type` in its content header and a `word_count` sidecar has
two metadata locations; a rule whose children ended up split across both edges has two child edges.
Real trees that predate a schema will contain both.

The three aspects are independent of each other. A node may have `Inside` content and a
`ParallelSidecar` — that is [C7](#c7)'s mixed layout, which is **observable but not intendable**.
The two-case simplification in §2 constrains what a schema can *declare*, not what a tree can
*contain*.

**These fields exist only for creating and editing.** Nothing in the read-side semantics of an
entity depends on them — not its identity, not its type, not its content or metadata values. A
consumer that only reads should never need to look at them.

### 4.2 Child edge is observed per rule, not per node

A node can legitimately carry children on both edges at once — a chapter with sections in its
directory and a `.notes` dot-child beside its file. So there is no single "this node's child edge"
to observe.

Actual edge is therefore resolved **per rule**: to add a child named `name`, match it to a rule,
then look at the edges of the existing children matching that same rule. All on one edge → that is
the actual edge. None → `None`, and the rule's declared edge applies. Split across both → ambiguous
(§4.5).

Resolving per rule is what makes the common case unambiguous. A chapter with sections on the slash
edge and notes on the dot edge has two conventions, but they belong to two different rules, so
neither rule is ambiguous. Only children matching the *same* rule and sitting on *different* edges
are.

If `name` matches an **existing child**, that child's own edge wins outright. Editing a child never
moves it.

### 4.3 Precedence when writing

For each aspect independently, in order:

1. **The thing already exists** — write where it is. Updating metadata on a node that has
   `S.meta.toml` writes `S.meta.toml`, whatever the schema says.
2. **The node has one actual convention for that aspect** (§4.1) — follow it.
3. **The node has several** — §4.5.
4. **The type declares an intent** (§2, §3) — follow it.
5. **Otherwise** — inherit layout from the parent (§2.1); edge defaults to `slash`.

A brand-new child reaches step 4 or 5 for every aspect, because none of it exists yet. An
established node rarely gets past step 2.

**Step 1 resolves at the finest granularity the aspect has.** This matters more than it looks, and
is what makes step 3 tractable:

| Aspect | What "the thing" is |
| --- | --- |
| Content | the node's content — one blob, so node-level |
| Metadata | a single **key**, not the metadata as a whole |
| Children | a single named **child**, not the child set |

So updating `word_count` on a node asks where `word_count` currently lives, not where the node's
metadata "generally" lives. Most apparent ambiguity never reaches step 3 because of this.

### 4.4 Intent disambiguates rather than rejects

Where the current code errors on an ambiguous pair, intended layout resolves it:

| On disk | Today | v2 |
| --- | --- | --- |
| `S.md` **and** `S/content.md` | hard error | the one matching intended layout is the content; the other is a reported stray |
| `S.notes.md` **and** `S/notes` | `Entity` yields two children; `LiveEntity::child()` hard-errors ([C2](#c2)) | **both are children.** They share a name but not an address; the collision is reported |

Only the first row needs disambiguating, and the reason is worth stating: a node has exactly one
`content()`, so two candidate content files means one of them is genuinely orphaned. Intent picks;
where intent does not disambiguate — both candidates conform, or neither does — it remains an error.

The second row is not the same shape. `S.notes` and `S/notes` are **distinct entities at distinct
addresses** that happen to share a name; `EntityPath` already tells them apart. Discarding one to
protect name-based lookup would make a file on disk unreachable through the API to save a single
accessor. So both load, and the ambiguity is pushed to the one operation that actually has it:

- `children()` returns both.
- `child(name)` **fails at runtime**, naming both paths. Not a policy decision — the name does not
  identify a child, so the call has no answer to give.
- `child_on_edge(name, edge)` is the unambiguous lookup, keyed on `(edge, name)`.

**`(edge, name)` is unique among a node's children** — equivalently, each child's final
`EntityPathEntry` is unique among its siblings. That is the invariant callers may rely on; `name`
alone is not a key.

### 4.5 Writing to an ambiguous node

§4.4 is about reading: which file *is* the content, which entry *is* the child. This section is
about writing to a node that legitimately has more than one convention established, which §4.4 does
not collapse and which is a normal state for a tree built before its schema existed.

The rule is the same for all three aspects: **route at the finest granularity that has an
unambiguous answer, and fall back to intent only for genuinely new things.**

**Metadata — route per key.** A node with `type` in its content header and `word_count` in
`S.meta.toml` has an ambiguous metadata location, but no ambiguity about either key. Updating
`type` rewrites the header; updating `word_count` rewrites the sidecar; deleting a key removes it
from wherever it lives. A **new** key has no home to route to, so it falls back to intent (§4.3
step 4) — and if intent's location does not exist yet, it is created.

This is the "something smart" case, and it is smart in a narrow, predictable way: nothing moves.
The library never relocates existing metadata as a side effect of writing an unrelated key, and it
never splits one logical update across two files.

Where the *same key* appears in two sources with different values, per-key routing has no answer
either. That is a conflict, and it is [D2](#d2)'s to resolve — not this section's.

**Children — route per name.** An existing child keeps its edge (§4.2), so a split rule creates no
ambiguity for any child that already exists. A **new** child under that rule falls back to the
rule's declared edge. The split is reported (§9) but does not spread: new children conform even
when their siblings do not.

**Content — cannot reach here.** §4.4 collapses `S.md` + `S/content.md` to a single content file
before any write is attempted, so content location is unambiguous by the time §4.3 runs.

**Ambiguity is never silently normalised.** A node that has drifted keeps its shape until someone
explicitly repairs it. Writing to such a node reports the ambiguity (§9.1) and then does the
narrowest correct thing — it does not take the opportunity to tidy up.

That guarantee is only tolerable because repair is available on demand: §9.2 defines an explicit
normalisation operation, so drift is sticky by choice rather than by omission. Without it, "nothing
moves" would mean a tree could only ever accumulate divergence.

---

## 5. The shape this must preserve

The single most important consequence of keeping layout and edge orthogonal:

```toml
[entity_types.chapter]
layout = "parallel"

[[entity_types.chapter.children]]
name_regex = '^\d{3}-'
node_type  = "section"
edge       = "slash"
```

```
chapters/000-the-invisible-kitchen.md          <- chapter content   (parallel layout)
chapters/000-the-invisible-kitchen/            <- chapter's children (slash edge)
    010-what-fermentation-is.md                <-   section content (parallel layout)
    review.md
```

A readable spine file, with a folder of children beside it. Every chapter in booker's
`demo-books/fermentation` has this shape, and its action scripts construct it deliberately
(`generate-section-outline.lua:32` derives the directory from the chapter's `.md` path).

Any design that couples layout to edge — including the tempting "a node is either a File node or a
Directory node" simplification — makes this shape inexpressible, forcing either
`chapters/000-x/content.md` (twenty identically-named files) or
`chapters/000-x.010-intro.md` (the hierarchy flattened into filenames). **This must remain a
required test case.**

---

## 6. Existence — unchanged

`try_load_entity` returns "no such entity" only when **all four** hold:

- no content file, and
- no metadata from any source, and
- no children, and
- no directory at `disk(P)`.

Equivalently, an entity **exists** if it has any of content, metadata, children, or a directory.
Three cases consumers get wrong, all of which remain valid:

- **A metadata-only entity is real** — `e.meta.toml` alone.
- **An empty directory is a real entity** — this is how container directories are modelled.
- **An intermediate dot-entity is real** — `ch1.notes.draft.md` alone makes `ch1.notes` exist,
  carrying nothing but the child `draft`.

---

## 7. Types

### 7.1 Resolution

The type a child loads as comes from the **parent's matching rule**, not from the child's own
metadata. A `type` key in metadata is a redundant, checked assertion: if present and different
from the expected type, that is an error.

Rule matching must have **exactly one implementation**. Today the load path
(`entity.rs:604-605`) open-codes the same regex walk that `Schema::child_type` (`schema.rs:37`)
performs, so the two can drift. See [C5](#c5).

### 7.2 `Auto` and `allow_additional`

- `Auto` — the type is read from the entity's own metadata `type` key. Absent metadata, a missing
  `type`, or a `type` of literally `"Auto"` are all errors.
- `allow_additional = true` — a child with no matching rule is permitted. It has **no rule**, so
  it has no declared edge and no declared type.
  - Its **layout** is inherited from its parent (§2.1).
  - Its **edge** at load time is whatever it is found on. At *creation* time, the caller must
    supply the type explicitly, and the edge defaults to `slash` unless specified.

### 7.3 Rule matching must not reject unbuilt names

Today a name that matches a rule but has no files behind it still errors: the rule match yields
`Ok(None)`, no other rule matches, and the "unexpected child" branch fires. A `.gitkeep` cannot be
handled by adding a rule for it — it must go in `ignore`.

In v2, **a matched rule that yields no entity is not an error.** The name simply produces no
child. See [C8](#c8).

### 7.4 `ignore`

`ignore` becomes a **regex list**, matching `children`. Having one field be exact-match and its
sibling be regex is a trap, and real trees repeat the same literals (`.gitkeep`, `README.md`,
`.gitignore`) in every type. See [C6](#c6).

---

## 8. Construction

The library owns all path decisions. No consumer should ever compute an entity's filename.

### 8.1 Rust

```rust
pub fn create_child(&self, name: &str) -> ChildBuilder;
```

The `EntityPathEntry` argument is **removed** — the caller supplies a name, and the edge is
resolved by §4.3. `ChildContentLayout` and its `Inferred` / `with_content_inside` /
`with_content_parallel` surface are **deleted**; layout is resolved by §4.3 too. See [C3](#c3),
[C4](#c4).

Note this is not simply "layout comes from the type": the builder consults the parent's *actual*
layout and edge first (§4.1), so a child added to a hand-built node inherits that node's existing
convention rather than normalising it. The schema is the fallback, not the first authority.

`with_type` is retained, and remains required when the slot is `Auto` or matched only by
`allow_additional`. Explicit per-aspect overrides may also be offered for callers that genuinely
want to force a location, but they should be the exception — the default path is §4.3.

`ChildBuilder` stays deferred — it accumulates and writes on `build()`, and keeps
`nested_children` so a whole subtree is composed and validated before anything touches disk. It
should be `#[must_use]`.

### 8.2 Scripting surface

The fluent form is easy to leave unsaved. The primary spelling is therefore a single table call:

```lua
chapter:add_child{
  name     = "review",
  type     = "chapter-review",   -- required only for Auto / allow_additional slots
  content  = review_text,
  meta     = { reviewed_at = now },
  children = { { name = "notes", content = "..." } },
}
```

One call, nothing to forget, and it maps field-for-field onto `ChildBuilder` including
`nested_children`. A fluent `:add_child(...):set_content(...):save()` form may exist as a secondary
spelling, but if it does, the host must track builders created during a script and **error after
the script returns if any were never written** — a deterministic check that does not depend on GC
timing.

---

## 9. Drift, and repairing it

Because reading is tolerant (§4), drift is not a failure mode to be guarded against — it is a
normal, expected state of a tree that humans edit. There is **no separate permissive reader**. The
ordinary read path already locates content by probing, already records actual layout (§4.1), and
already knows the intent from the schema. Reporting drift is therefore a comparison over
information the loader has anyway, not a second traversal with different rules.

But detection alone leaves drift sticky: §4.5 guarantees nothing is normalised as a side effect of
writing, so without an explicit repair operation a tree can only accumulate divergence. §9.2 is
that operation.

### 9.1 What is reported

Reported per node — nonconformance, where an aspect resolved to one value that disagrees with
intent:

- actual content location vs intended (§4.1) — `ContentLocationNonconformance`,
- actual metadata location vs intended — `MetadataLocationNonconformance`,
- actual child edge vs intended, per rule (§4.2) — `EdgeNonconformance`,
- the content file left over from §4.4's disambiguation — `StrayContent`.

And **ambiguity**, where an aspect resolved to several values at once (§4.5) — a distinct finding,
because it is not "this node disagrees with the schema" but "this node does not agree with itself":

- metadata split across two or more sources, with which keys live where — `SplitMetadata`,
- a rule whose children are split across both edges — `SplitChildEdge`,
- the same metadata key in two sources with different values, the one case §4.5 cannot route,
  subject to [D2](#d2) — `MetadataKeyConflict`,
- two children sharing a name across edges (§4.4) — `DuplicateChildName`.

#### Three questions a child is asked

These are easy to conflate, so: a name arriving from the discovery passes is asked three
independent questions, and each has its own finding.

| Question | Finding | Example on disk |
| --- | --- | --- |
| Does the schema allow a child by this name at all? | `UnexpectedChild` | `stray` under `allow_additional = false` |
| Do two children share this name across edges? | `DuplicateChildName` | `ch1.notes.md` **and** `ch1/notes.md` |
| Is this one node with two content files? | `StrayContent` | `ch1.md` **and** `ch1/content.md` |

The first is about the schema, the second about two entities, the third about one entity. Only the
third discards anything.

Two ordering rules follow:

- `UnexpectedChild` is reported **once per name, not once per edge**. The name is what the schema
  rejects; which edge it was found on is irrelevant to that judgement.
- `UnexpectedChild` **short-circuits**. A rejected name never reaches the duplicate check, so a name
  never produces both findings — where a child that shouldn't exist doesn't exist is noise.

The distinction matters to a repair tool: nonconformance has an obvious fix (move the file to where
intent says), while ambiguity requires choosing which of two existing homes wins, and may need a
human.

Two consequences worth stating plainly:

- **Layout is not required to read a tree.** Content is found by probing both locations, as it is
  today; intent only breaks ties (§4.4) and feeds this report. A project whose schema is missing or
  wrong is degraded, not unreadable — types and validation are lost, but the tree still opens.
- **Drift does not fail a load.** Whether it fails a *command* is a consumer's policy decision, not
  the library's.

### 9.2 Normalisation

Rewriting a node so that it conforms to a layout. The target is **named explicitly**; conforming to
the schema is the common case and gets a convenience, but it is not the only option and not
privileged in the API.

```rust
/// Which aspects to change. `None` on a field means "leave this aspect alone".
pub struct NormaliseSpec {
    pub content:    Option<ContentLocation>,
    pub metadata:   Option<MetaLocation>,
    pub child_edge: Option<Vec<(RuleRef, Edge)>>,
}

impl LiveEntity {
    /// Compute the moves without performing them.
    pub fn plan_normalise(&self, spec: &NormaliseSpec) -> anyhow::Result<MovePlan>;
    /// Validate and execute.
    pub fn normalise(&mut self, spec: &NormaliseSpec) -> anyhow::Result<MoveReport>;
    /// Convenience: fill every field from what the schema intends for this node.
    pub fn normalise_to_schema(&mut self) -> anyhow::Result<MoveReport>;
}
```

Four properties this must have:

**Per-aspect and independent.** The spec mirrors §4.1's three aspects, each optional. Converting a
node's content from `S.md` to `S/content.md` does not move its children, in either direction —
layout and edge are orthogonal (§5), and they stay orthogonal under repair. `content.md` and
`meta.toml` are reserved names skipped by the slash pass, so moving content *into* a directory that
already holds slash children cannot collide with them.

**Plan before write.** `plan_normalise` returns the full set of moves without touching disk.
This is not a convenience: validation has to happen before the first rename (see atomicity below),
and a repair tool needs to show the user what it is about to do. `normalise` is `plan` plus
execute.

**Collisions block; nothing is overwritten.** Moving `S.notes.*` to `S/notes.*` when `S/notes.md`
already exists is [C2](#c2)'s ambiguity showing up as a repair conflict. The plan fails validation
and no file moves. The same applies to normalising metadata into a location that already holds
different values for the same key — that is [D2](#d2)'s conflict, and it blocks.

**Merging is part of it.** Normalising metadata on a node with both a header and a sidecar means
combining them into the target. Non-conflicting keys merge; a conflicting key blocks the whole
operation rather than picking a winner.

### 9.3 Relocation is not normalisation

These get confused, and the existing `move_to` (`live_entity.rs:892`) shows why: moving a node
already drags its whole subtree along. It renames the directory, then scans the parent for
everything prefixed `S.` and rewrites each to the new stem. A dot-child's own dot-children come
with it because `S.notes.draft.md` shares the prefix.

That is **relocation** — mechanical, forced, and non-negotiable: a node's descendants live at paths
derived from its own, so moving it must move them. Their *layouts* are untouched; a dot-child that
was `Parallel` before is `Parallel` after, at a new stem.

**Normalisation** is a per-node choice about layout. It does not recurse. Normalising a subtree
means normalising each node in it, which a caller opts into node by node — most naturally by
walking §9.1's report and repairing the nodes it names.

Keeping these separate is what makes [D5](#d5) answerable: a move relocates and preserves layout;
if the result no longer matches intent, that is reported, and normalising it is a second, explicit
step.

### 9.4 Atomicity

Normalisation mutates several files. A failure part-way leaves a node in a state that is worse than
the drift it was fixing — half its metadata moved, or content relocated with children left behind —
and that state is new ambiguity (§4.5) rather than mere nonconformance.

The current code offers no help here. `move_to` is a bare sequence of `fs.rename` calls with a
`moved_anything` flag and no rollback; if the third rename fails, the first two stand.

Mitigation is the plan/execute split: validate the entire plan against the filesystem first, so
that the common failures — collisions, missing sources, conflicting keys — are caught before
anything moves. That leaves only genuine I/O failure mid-execution, which cannot be prevented
without staging. Whether it must be *survivable* is [D8](#d8).

---

## 10. What the current code gets wrong

Every item below is a defect in the code as of `storage-layout.md`'s baseline. They are listed
here so the v2 implementation can be checked against them: **each one needs a test that fails
against today's behaviour and passes against this spec.**

### C1 — Dot-child metadata sidecar is substituted, not appended

`entity.rs` derives two sibling files by different rules, four lines apart:

```rust
// entity.rs:480  content  — APPENDS:     a/b.review -> a/b.review.md
let dot_content_file  = entity_path.to_pathbuf(base_path).with_added_extension("md");
// entity.rs:516  metadata — SUBSTITUTES: a/b.review -> a/b.meta.toml
let dot_metadata_file = entity_path.to_pathbuf(base_path).with_extension("meta.toml");
```

Under substitution the dot-child `a/b.review` and its parent `a/b` resolve to the **same** metadata
file, so one file supplies both entities' metadata wholesale — including the parent's `type`, which
then fails the child's own type check at `entity.rs:585`.

Three sites substitute: `entity.rs:516` (reader), `entity.rs:693` (`EntityWriter::write_entity` —
and children are written *after* the parent, so a dot-child with parallel metadata **overwrites its
parent's file**), and `live_entity.rs:912` (rename). Two sites append: `live_entity.rs:360` and
`:1000`.

`live_entity.rs:1726`, `test_with_metadata_parallel_dot_child`, asserts the appended form and
passes today — so `EntityLoader` already cannot read back what `LiveEntity` writes.

**v2 requires:** appended, everywhere. §2.

**Tests:** round-trip a dot-child with a parallel sidecar through writer then reader; assert the
parent's sidecar is untouched after writing a dot-child; assert reader and writer agree on the
path.

### C2 — Load and lookup disagree about the same name on both edges

```rust
// live_entity.rs:1008 — lookup: hard error
(true, true) => bail!("Both Slash and Dot child exist with name '{}'", name),
```
```rust
// entity.rs:551-553 — load: chained, no cross-pass dedup
let children = dot_children.into_iter().chain(slash_children).collect::<Vec<EntityPath>>();
```

`Entity` loading accepts both and yields **two** children with the same name, each dispatched
against the same rules and loaded independently. `LiveEntity::child()` refuses to resolve the name
at all. Two components actively contradict each other.

**v2 requires:** one resolver, called by both, and neither of today's outcomes. Both files are
children — distinct addresses sharing a name (§4.4) — so `children()` returns two and reports
`DuplicateChildName`. `child(name)` still fails, but for a stated reason and naming both paths,
rather than as an accident of a lookup that never consulted the schema. §3.1.

Note that what was actually broken here was not "two children came back". It was that `children()`
returned two *silently*, from code unrelated to the code `child()` used, with nothing recorded
anywhere. Agreement comes from the shared resolver, not from making both return one thing.

**Tests:** create `ch1.notes.md` and `ch1/notes.md`; assert `children()` yields two children with
distinct `EntityPath`s and equal names, that `DuplicateChildName` is reported naming both disk
paths, that `child("notes")` errors naming both, and that `child_on_edge("notes", Edge::Slash)`
returns the slash one.

### C3 — The writer guesses layout from the edge

```rust
// live_entity.rs:44-51
enum ChildContentLayout {
    /// Layout is chosen automatically: Slash entries use Inside, Dot entries use Parallel.
    Inferred,
    ...
}
```

This heuristic exists only because nothing tells the builder the answer. It is also the exact
coupling that makes §5's shape unwritable through the library — which is why booker bypasses
`LiveEntity` entirely and writes files by hand.

**v2 requires:** deleted. Layout comes from the resolved type. §8.1.

**Tests:** build §5's chapter/section tree entirely through the builder API and assert the on-disk
result matches the tree in §5 byte for byte.

### C4 — The schema cannot distinguish dot from slash

`ChildEntityRules` is `{ name_regex, node_type, required, multiple }` — no edge field. The edge is
discarded at both places rules are consulted:

```rust
// entity.rs:599 — load path; Dot/Slash collapsed by last_name() (entity.rs:65-66)
let child_name = child_entity_path.last_name().unwrap();
```
```rust
// live_entity.rs:1011, :1020 — lookup path, both branches identical
let node_type = entity_type_descriptor.child_type(name)?;
```

So a rule `name_regex = "review"` matches `S.review` and `S/review` identically, and the schema
cannot express "sections go in the directory, notes hang off the file".

**v2 requires:** `edge` on `ChildEntityRules`, available wherever rules are consulted. §3. It is
*consulted* at load, not *enforced* — a nonconforming child still loads and is reported (§4).

**Tests:** a schema declaring `edge = "slash"` must place a *new* child on the slash edge; must
still load an existing dot-child of that name; and must report that dot-child as nonconforming.

### C5 — Rule matching has two implementations

`Schema::child_type` (`schema.rs:37`) walks the rules with a regex match. The load path
(`entity.rs:604-605`) open-codes the same walk instead of calling it. They can drift, and one of
them compiles its regex with `.unwrap()`.

**v2 requires:** one implementation, called from both. §7.1.

**Tests:** a schema with an invalid regex must produce the same error from load and from lookup.

### C6 — `ignore` is exact-match while `children` is regex

`entity.rs:600` compares `ignore` entries with `==`. Every type's list repeats the same literals.

**v2 requires:** regex. §7.4. Note this silently changes the meaning of existing entries
(`".config"` becomes an unanchored pattern) — acceptable, since compatibility is not a goal, but
the migration must rewrite existing lists as anchored patterns.

### C7 — Mixed content/metadata layouts are permitted and undocumented

`e.md` + `e/meta.toml` and `e/content.md` + `e.meta.toml` both load today. Neither is documented.
The first arises naturally whenever a parallel node's directory holds its slash children and
someone drops a `meta.toml` in.

**v2 requires:** un**intend**able but still readable. A schema cannot declare a mixed layout (§2),
and a node observed in one is reported as nonconforming (§9) — but it loads, and anything written
to it afterwards follows the mix it already has (§4.3). Documented as an observable state, not as a
supported declaration.

**Tests:** each mixed combination loads with correct content and metadata; each is reported as
nonconforming; and adding metadata to a node with `e.md` + `e/meta.toml` writes to `e/meta.toml`
rather than normalising to `e.meta.toml`.

### C8 — A matched rule with no files behind it errors

A name matching a rule yields `Ok(None)`; no other rule matches; the "unexpected child" branch
fires. So a rule cannot be used to tolerate a placeholder — only `ignore` can.

**v2 requires:** not an error. §7.3.

**Tests:** a `.gitkeep` covered by a `children` rule loads cleanly.

### C9 — `required` and `multiple` are parsed and never enforced

Declared on `ChildEntityRules` (`schema.rs:16-19`), read by no non-test code. A schema author
writing `required = true` gets nothing.

**v2 requires:** a ruling — see [D1](#d1).

### C10 — Malformed sidecar is fatal; malformed header is silent

A `.meta.toml` that fails to parse aborts the entire load (`entity.rs:278` propagates the
`toml::from_str` error). A content file whose front matter fails to parse is silently treated as
having no metadata, with the unparsed text becoming the body. The same authoring mistake either
stops everything or passes unnoticed, depending only on where the author put it.

**v2 requires:** a ruling — see [D3](#d3).

### C11 — `README.md:35` misdescribes root metadata

The README labels `project.meta.toml` "Metadata for the root project (Parallel)". The parallel
sidecar is gated on `!is_root` (`entity.rs:517`), and README:63 itself says the root has only slash
children. Under v2 the root is always `inside`, so root metadata is `meta.toml` inside the root
directory, and a file named `project.meta.toml` there is a metadata-only **child** named `project`.

**v2 requires:** the README line rewritten alongside the implementation.

---

## 11. Open decisions

These are not settled. They need a ruling before implementation, and each changes what the tests
assert.

### D1 — Enforce or delete `required` / `multiple`?

If enforced: as hard load errors, or as reported violations surfaced through §9's diagnostic
channel? Reported violations fit better with a tree humans edit by hand — a missing required child
is a normal intermediate state during authoring, not a corrupt repository.

*Leaning: enforce as reported violations, not errors.*

### D2 — Merge in-header and sidecar metadata, or keep them exclusive?

Today supplying both is an error. Merging them when no key conflicts would let `type` and `id`
stay legible in the file header while tools write bulk derived metadata to the sidecar.

The write-back cost that made this a hard trade is largely paid off by §4.5: per-key routing
answers "update a key where it already lives," and new keys fall back to intent. What merging still
costs is that it dissolves an error catching a real mistake — metadata moved to a sidecar with the
header not stripped. §9 answers that too, by reporting split metadata as *ambiguity* rather than
accepting it silently.

So the residue is narrow. Two things remain genuinely open:

- **Key conflict.** The same key in two sources with different values is the one case per-key
  routing cannot resolve. Error, or does one source win by a stated precedence?
- **Is split metadata worth reporting at all**, or is it a legitimate authoring style — header for
  hand-edited identity, sidecar for tool-written derived data? §9 currently assumes it is worth
  reporting.

Note this is orthogonal to layout: in-header metadata lives in the content file wherever layout put
it. Layout does not decide D2.

*Leaning: merge; per-key routing per §4.5; new keys follow intent; split metadata reported as
ambiguity per §9; key conflict is an error.*

### D3 — Should malformed sidecars and malformed headers agree in severity? (C10)

And in which direction? Making both non-fatal fits the drift-tolerant posture of §9; making both
fatal is safer against silent data loss.

*Leaning: both non-fatal, both reported as drift, with strict mode escalating to an error.*

### D4 — How are nonconformance reports surfaced?

Largely dissolved by §4. There is no separate permissive reader to design, and actual layout hangs
off the entity by construction, so the remaining question is narrow: are findings accumulated into
a list on the load result, or derived on demand by a consumer that walks the tree comparing actual
against intended?

*Leaning: accumulate during load. The loader already visits every node and already knows both
facts; making a consumer re-derive it invites two implementations, which is exactly [C5](#c5).*

### D5 — Does actual layout survive a move or rename?

Moving `a/b` to `c/d` where the two ends have different intended layouts: does the node keep the
layout it had, or normalise to its new type's intent?

Keeping it is consistent with §4 — a move should not silently restructure files. Normalising is
what someone reorganising a tree probably wants. The answer may differ between a move within a type
and a move that changes type.

§9.3 mostly settles this by separating the two operations: a move **relocates**, which necessarily
drags descendants but touches no layout; normalising afterwards is a second, explicit call. A
caller who wants both can make both calls, and `move_to` needs no policy of its own.

*Leaning: keep. Relocate, report, and leave normalisation to §9.2.*

### D6 — Is there a strict mode at all?

§4 makes nonconformance always-tolerated at the library level. Some consumer may still want "refuse
to open a tree that does not conform" — CI, say, or a scripted batch run where silent drift would
compound.

If yes, is it a load-time flag in the library, or purely a consumer-side policy applied to the
report? The latter keeps the library's semantics single and is much less code.

*Leaning: consumer-side policy. The library reports; it does not adjudicate.*

### D7 — What happens to a stray during normalisation?

§4.4 picks one of `S.md` / `S/content.md` as the content and calls the other a stray. Normalising
the node then has to do something with that file. Three options: **refuse** until the user removes
it, **delete** it, or **preserve** it under a name outside the entity model (`S.md.orphaned`, say).

Deleting is the only one that leaves a clean tree unattended, and it is the only one that can
destroy someone's writing. Refusing is safe but means the most common real drift cannot be repaired
by the automatic path, which undercuts the point.

*Leaning: refuse by default, with an explicit opt-in that preserves rather than deletes. Never
delete content the library did not write.*

### D8 — Does normalisation need to survive an I/O failure mid-execution?

§9.4's plan/execute split catches collisions, missing sources and key conflicts before any file
moves. What it cannot catch is a rename failing part-way — a full disk, a permissions change, a
process killed. The node is then left half-converted, which is new ambiguity rather than the
nonconformance it started as.

Options: accept it and return a `MoveReport` precise enough to finish or reverse the operation by
hand; make execution reversible by recording an undo log; or stage the whole thing (write to
temporaries, then swap) which `Xfs` does not currently support.

The cost is very different between them, and the risk depends on how large a plan typically is —
for a node with a handful of files, brief; for normalising a large subtree node by node, much less
so.

*Leaning: a precise `MoveReport` for the first cut, with staging deferred until there is evidence
it is needed. This should be a conscious decision rather than an omission.*
