# Storage Layout

How an entity finds its **content**, its **metadata**, and its **children** on disk.

The variety of shapes here is deliberate. A note attached to a node should be able to be
`my_node.my_note.md` with inline TOML — one file — rather than forcing
`my_node/my_note/content.md` plus `my_node/my_note/meta.toml`. Equally, a chapter should be able
to be a readable `chapter.md` with a `chapter/` folder of sections beside it. The rules below
exist to make those choices free, and to make each of them a **declaration in the schema** rather
than something a caller has to know.

Sections are numbered so the code can cite them; modules and tests refer to `§n` throughout.

---

## 1. Identity: logical paths and disk paths

An entity is identified by an `EntityPath`: an ordered list of entries, each either
`Slash(name)` or `Dot(name)`. The **root** has an empty entry list.

`disk(P)` — the entity's **stem** — folds the entries over the base path:

| Entry | Stem |
| --- | --- |
| `Slash(name)` | `disk(parent)/name` — appends a path component |
| `Dot(name)` | `disk(parent).name` — appends to the last component |

A stem **never carries an extension**. Every file an entity owns is built from its stem by
**appending** a suffix or by descending into it:

```
book/chapters/ch1              the stem of "chapters/ch1"
book/chapters/ch1.md           its content, parallel
book/chapters/ch1.meta.toml    its sidecar, parallel
book/chapters/ch1/content.md   its content, inside
book/chapters/ch1/meta.toml    its sidecar, inside
```

Appending rather than substituting is what keeps a dot-child distinct from its parent. The
dot-child `chapters/ch1.review` has the stem `book/chapters/ch1.review` and therefore the sidecar
`book/chapters/ch1.review.meta.toml`. Substituting the extension would give it
`book/chapters/ch1.meta.toml` — its *parent's* file — so writing the child would silently destroy
the parent's metadata.

`src/placement.rs` is the single authority for this arithmetic. Nothing else builds a path.

**An entity name may not contain a `.`.** `my.file.md` is the entity `my` with the dot-child
`file`. A name that cannot be read this way is rejected rather than silently mangled.

## 2. Layout — where a node's own files live

An entity's **layout** decides where its content and its sidecar sit, relative to its stem `S`:

| `Layout` | Content | Sidecar |
| --- | --- | --- |
| `Parallel` | `S.md` | `S.meta.toml` |
| `Inside` | `S/content.md` | `S/meta.toml` |

Layout is declared on the **entity type** (`EntityTypeDescription.layout`).

The **root** is always `Inside`. It has no parent filename to sit beside, so a root type
declaring `layout = "parallel"` is a schema error — no tree can be shaped that way, and there is
nothing on disk to tolerate.

### 2.1 Layout is inherited by instance

A type that declares no `layout` takes the layout of the parent **instance** it was reached
under — not of the parent's *type*. The same type can therefore be parallel in one part of a tree
and inside in another, and switching one type's declaration switches the whole subtree beneath it.

The eager loader threads it down as `inherited_layout`; the lazy handle carries it as
`LiveEntity.inherited_layout`. It is derived from schema and ancestry only, never from a disk
probe, so it cannot go stale. `reading::resolve_layout` is the one implementation.

## 3. Edge — how a child attaches to its parent

A child's **edge** decides how its stem is built from its parent's:

| `Edge` | Stem |
| --- | --- |
| `Slash` | `disk(parent)/name` |
| `Dot` | `disk(parent).name` |

Edge is declared on the **parent's child rule** (`ChildEntityRules.edge`), and defaults to
`Slash` when the rule omits it — dot is the compact case you opt into deliberately.

The root has no dot children: there is no filename to prefix. A root type declaring
`edge = "dot"` on any rule is a schema error.

**Layout and edge are orthogonal.** The parent's rule says where the child attaches; the child's
type says where the child's own files go. Nothing infers one from the other, which is what makes
§5's shape expressible.

### 3.1 Discovery

Two passes look for children:

- the **dot pass** scans the parent directory for entries whose filename begins `stem.`;
- the **slash pass** scans the stem directory itself.

Each yields **names**, not paths — a name may be seen by both passes, and committing to an edge
inside a pass would make that state unrepresentable.

Reserved names the slash pass skips, because they belong to the parent rather than to a child:
`content.md`, `meta.toml`, `schema.toml`. Reserved suffixes the dot pass skips, because they are
the entity's own files: `md`, `meta.toml`. The dot pass does not run for the root.

## 4. Reading

### 4.1 What is observed

`reading::read_node` reads one node: it probes both content locations and both sidecar locations,
resolves the type, resolves the layout, and only then decides which content file wins. Both the
eager `EntityLoader` and the lazy `LiveEntity` resolve through it, which is what makes them agree
rather than merely intend to.

The ordering is a little circular and matters. Choosing between two content files needs the
intended layout, which needs the type, which may itself be recorded in the losing file's front
matter. So: probe existence, resolve the type, decide, then read. Where both content files exist
the type is resolved from the sidecars alone; where only one exists there is nothing to choose,
so its front matter takes part in typing normally.

A node's metadata may live in three places, and the three are described by `MetaLocation`:
`InHeader` (front matter in whichever file holds the content), `ParallelSidecar` (`S.meta.toml`)
and `InsideSidecar` (`S/meta.toml`).

### 4.2 Children, and the edge observed per rule

`discovery::resolve_children` is the single child resolver. It merges the two passes' name lists
*before* consulting the schema, so a name is never resolved twice, then asks
`CompiledType::match_child` once per name (§7).

Edge observation is **per rule**, not per node. Two rules on different edges are not a split — that
is the normal case, and §5 depends on it. One rule whose children sit on both edges *is* a split,
and is reported.

### 4.3 Where a write lands

Writing routes to what the node has **already established** on disk. Only a node with nothing
there follows its type's intent.

For **content**:

1. the location of the content file that already exists;
2. failing that, `layout.content_location()`.

For **one metadata key**:

1. the source that already holds that key;
2. failing that, the node's only source, if it has exactly one;
3. failing that, `layout.sidecar_location()`.

Step 2 is what keeps a mixed node mixed: a new key joins the sidecar the node actually has, rather
than creating the one its layout would have chosen. Writing never normalises — a save is not the
place to restructure a tree.

### 4.4 When observation and intent disagree

Reading does not fail on a tree that disagrees with its schema. Trees are edited by hand, and
drift is normal.

- Both `S.md` and `S/content.md` exist: intent picks, and the loser is reported as a
  `StrayContent` finding. Neither is deleted.
- A name is found on both edges: these are two distinct entities that merely share a name, and
  **both** are returned. The collision is reported as a `DuplicateChildName` finding, and a
  name-based `child()` lookup fails at the call, naming both files.
- Content or a sidecar sits somewhere the layout did not intend: it loads, and the difference is a
  `ContentLocationNonconformance` or `MetadataLocationNonconformance` finding. A node whose two
  halves follow different layouts is legal, loads correctly, and stays mixed.
- A metadata file does not parse: the source is **retained** in a `Malformed` state carrying its
  raw text and the parse error, and reported. It never aborts a read, it is written back verbatim,
  and it can be repaired by replacing it outright. A load/save cycle cannot destroy a file the
  library could not read.

### 4.5 Metadata is a list of sources

`EntityMeta` is the set of sources observed on disk, normalised into `MetaLocation` order
(`InHeader` < `ParallelSidecar` < `InsideSidecar`) so merge precedence is deterministic regardless
of probe order.

Sources **merge per key**. Disjoint keys combine; the same key with the same value in two sources
is not a conflict; the same key with *different* values is a `MetadataKeyConflict`, which the
default policy rates `Error`. Where the policy downgrades it, the later location wins.

`location_of(key)` is what routes a write (§4.3), and it is why setting one key never moves
another.

In-header metadata is orthogonal to layout: front matter lives in whichever file holds the
content, so it is never "misplaced". Only a sidecar can be.

## 5. The shape this exists to preserve

```
book/chapters/000-the-invisible-kitchen.md      the chapter's own content
book/chapters/000-the-invisible-kitchen/        its sections, beside it
    010-what-fermentation-is.md
    review.md
```

A readable spine file with a folder of children next to it. The chapter's type declares
`Parallel`; the chapter's rules put its children on the `Slash` edge. That combination is only
expressible because layout and edge are separate declarations — an implementation that derived one
from the other would force either `000-the-invisible-kitchen/content.md` (losing the readable
filename) or `000-the-invisible-kitchen.010-what-fermentation-is.md` (losing the folder).

## 6. When is there a node here at all?

A node exists if it has content, **or** metadata, **or** children, **or** a directory.

An `Inside` node's directory is its trace even when empty, so creating one with nothing else to
write is allowed. A `Parallel` node with nothing to write would leave nothing on disk at all, so
it is refused: a node is what it leaves behind.

## 7. Types and rules

`CompiledType::match_child` is the **only** implementation of rule matching. Every site that needs
to know a child's type or edge goes through it, so no two callers can reach different conclusions
about the same name. It returns one of four outcomes: `Ignored`, `Matched`, `Additional`,
`Unexpected`.

Every regex in a schema — in `children` rules and in `ignore` alike — is compiled once, when the
type is added. `Schema::add_entity_type` therefore fails on an invalid pattern, rather than
letting one call site error on it and another silently skip it.

### 7.1 Resolution

A child's type comes from the rule that matched its name. If the node's own metadata records a
`type`, it is checked against that: a disagreement is a `TypeMismatch` finding, rated `Error` by
default.

### 7.2 `Auto` and `allow_additional`

A child matching no rule under `allow_additional = true` is `Additional`. It has no rule, and
therefore **no declared type and no declared edge**. Its type is `Auto`, which means the type must
be recorded in its own metadata; creating one requires `with_type`.

A child matching no rule under `allow_additional = false` is `Unexpected`, reported as such.

### 7.3 Rules match names, not entities

A rule matching a name is not a claim that an entity is there. A name that matches a rule but has
nothing behind it resolves without complaint and simply produces no entity — which is what lets a
rule match `.gitkeep`, or a name a caller is about to create.

`required` and `multiple` are **reported, never enforced**. A rule marked `required` with no
matching child yields a `MissingRequiredChild` finding; a rule not marked `multiple` with several
matching children yields `MultipleChildrenNotAllowed`. Every child still loads.

### 7.4 `ignore`

`EntityTypeDescription.ignore` is a list of **regexes**, matched against child names the same way
`children` rules are. A matching name produces no child at all, and is not an error — it is how a
tree carries non-entity files and tool directories without the schema complaining about them.

## 8. Construction

`LiveEntity::create_child` takes a **name** and returns a `ChildBuilder`. The caller never builds
a path.

`build()` resolves, in order:

1. **Type** — from `match_child` on the parent's compiled type.
2. **Edge** — an existing child of that name keeps its edge; otherwise, if every existing child of
   the *same rule* sits on one edge, the new child joins them; otherwise the rule's declaration
   wins, so a split never spreads.
3. **Layout** — the child type's declaration, falling back to the parent instance's layout (§2.1).
4. **Paths**, from `placement`, and then the write.

Nested children are built with the resolved layout as their inherited layout, so a subtree built in
one call is consistent with the node it hangs from.

### 8.1 Overrides

`with_edge(Edge)` and `with_layout(Layout)` override each aspect independently, and `with_metadata_at`
places a sidecar explicitly. They are the escape hatch, not the ordinary path: the point of the
schema declaring these things is that most callers never touch them.

Because layout and edge are resolved separately, they can be overridden separately — forcing a
child onto the dot edge says nothing about where its own files go.

## 9. Drift

### 9.1 What is reported

Two different things are reported, through one channel.

**Nonconformance** is "this node disagrees with its schema". It has an obvious fix, and a tree
humans edit by hand is full of it.

**Ambiguity** is "this node does not agree with itself". There is no resolution the library can
pick without inventing one.

`Entity.findings` carries what the eager loader saw on that node, with `Entity::all_findings()` to
walk the subtree. `LiveEntity::issues()` computes the same for one node at call time, and never
fails on a finding whatever the policy says — reporting is its whole job.

### 9.2 Severity

Severity is configurable **per finding kind** through a `FindingPolicy`:

| `Severity` | Effect |
| --- | --- |
| `Ignore` | not recorded at all |
| `Warn` | recorded |
| `Error` | recorded, and the call that produced it returns `Err` |

Nonconformance defaults to `Warn`; ambiguity with no defined resolution — a metadata key with two
different values, a child name found on both edges, a type that contradicts its rule — defaults to
`Error`. `FindingPolicy::strict()` makes every kind an error, for CI; `silent()` records nothing,
for consumers that only want the data. `with(kind, severity)` overrides one kind.

The two readers differ in when this bites. `EntityLoader` walks the whole tree, so an `Error`
fails the load up front. `LiveEntity` is lazy, so an `Error` fails only the accessor that produced
it: `metadata()` can fail on a malformed sidecar while `children()` still succeeds.

### 9.3 Relocation is not normalisation

`move_to` **relocates and does not normalise**. Every file keeps the layout it arrived with, at
the new stem, and dot-descendants are dragged along because their paths derive from the moved
node's.

If the node's new position intends a different layout, that is a finding on the moved node, not
something the move repairs. A moved handle's inherited layout is whatever it was before the move,
so re-fetch through the new parent's `child()` if the node changed parents.

---

## Not implemented

Normalisation — planning and executing a set of moves that bring a drifted tree back into
conformance, with collision checking and metadata merging on repair — is designed but not built.
`move_to` is the primitive it would be composed from.
