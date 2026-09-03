# Storage Layout — As-Built Description

> Describes what the code **does**, as of the v2 core implementation. The target design and the
> reasoning behind it are in [`storage-layout-v2.md`](storage-layout-v2.md); section numbers here
> match that document so the two can be read side by side. Where they disagree, this file is the
> one that is wrong — say so, and fix it.

How an entity finds its **content**, its **metadata**, and its **children** on disk.

The variety of layouts is deliberate. A note attached to a node should be able to be
`my_node.my_note.md` with inline TOML — one file — rather than forcing
`my_node/my_note/content.md` plus `my_node/my_note/meta.toml`. The rules below exist to make that
choice free, and to make it a *declaration* rather than something a caller has to know.

---

## 1. Logical paths and disk paths

An entity is identified by an `EntityPath`: an ordered list of entries, each either
`Slash(name)` or `Dot(name)`. The **root** has an empty entry list.

`disk(P)` — the entity's **stem** — folds the entries over the base path:

- `Slash(name)` appends a path component: `disk(parent)/name`
- `Dot(name)` appends to the last component: `disk(parent).name`

A stem **never carries an extension**. Every file an entity owns is built from its stem by
**appending** a suffix or by descending into it. Substituting an extension would resolve the
dot-child `a/b.review` onto its parent's `a/b.meta.toml`; that was defect C1, and it is why
`src/placement.rs` is the single authority for this arithmetic and why `with_extension` appears
nowhere in `src/`.

An entity name may not contain a `.`. `my.file.md` is the entity `my` with the dot-child `file`.

## 2. Layout — where a node's own files live

An entity's **layout** decides where its content and its sidecar sit, relative to its stem `S`:

| `Layout` | Content | Sidecar |
| --- | --- | --- |
| `Parallel` | `S.md` | `S.meta.toml` |
| `Inside` | `S/content.md` | `S/meta.toml` |

Layout is declared on the **entity type** (`EntityTypeDescription.layout`), not on the instance
and not on the parent's rule.

### 2.1 Inheritance is by instance

A type that declares no `layout` takes the layout of the parent **instance** it was reached
under — not of the parent's *type*. The eager loader threads it down as `inherited_layout`; the
lazy handle carries it as `LiveEntity.inherited_layout`, which is derived from schema and
ancestry only, so it cannot go stale the way a disk snapshot would. `resolve_layout` in
`src/reading.rs` is the one implementation.

The **root** is always `Inside`. A root type declaring `layout = "parallel"` is a schema error,
not drift: the root has no parent filename to sit beside, so no tree can be shaped that way.

## 3. Edge — how a child attaches to its parent

A child's **edge** decides how its stem is built from its parent's:

| `Edge` | Stem |
| --- | --- |
| `Slash` | `disk(parent)/name` |
| `Dot` | `disk(parent).name` |

Edge is declared on the **parent's child rule** (`ChildEntityRules.edge`), and defaults to
`Slash` when the rule omits it — dot is the compact case you opt into deliberately.

The root has no dot children — there is no filename to prefix — so a root type declaring
`edge = "dot"` on any rule is a schema error.

**Layout and edge are orthogonal.** The parent's rule says where the child attaches; the child's
type says where the child's own files go. Coupling them (the old `ChildContentLayout::Inferred`
heuristic: slash ⇒ inside, dot ⇒ parallel) made §5's shape unwritable through the library, which
was defect C3.

## 4. Reading

### 4.1 Observation

`reading::read_node` reads one node: it probes both content locations and both sidecar locations,
resolves the type, resolves the layout, and only then decides which content file wins. Both the
eager `EntityLoader` and the lazy `LiveEntity` resolve through it, which is what makes them agree
rather than merely intend to.

The ordering matters and is a little circular: choosing between two content files needs the
intended layout, which needs the type, which may itself be recorded in the losing file's front
matter. Where both files exist the type is resolved from the sidecars alone; where only one
exists there is nothing to choose, so its header takes part in typing normally.

### 4.2 Children

`discovery::resolve_children` is the single child resolver. Two passes run — the **dot pass**
scans the parent directory for entries prefixed `stem.`, the **slash pass** scans the stem
directory — and each yields *names*, not paths, because a name may be seen by both.

Reserved names the slash pass skips: `content.md`, `meta.toml`, `schema.toml`. Reserved suffixes
the dot pass skips: `md`, `meta.toml`. The dot pass does not run for the root.

The two name lists are merged before the schema is consulted, so a name is never resolved twice.
For each name, `CompiledType::match_child` decides once: `Ignored` (matched an `ignore` regex —
no child, no error), `Matched` (the rule supplies the type and the intended edge), `Additional`
(`allow_additional = true`; no rule, so no declared type and no declared edge) or `Unexpected`.

Edge observation is **per rule**: two rules on different edges are not a split, but one rule whose
children sit on both edges is, and is reported.

`required` and `multiple` are reported, never enforced — a tree that violates them still loads.

### 4.3 Write precedence

Writing routes to what the node has **already established** on disk. Only a node with nothing
there follows its type's intent:

- **Content** — the location of the content file that exists; failing that, `layout.content_location()`.
- **One metadata key** — the source that already holds that key; failing that, the node's only
  source if it has exactly one; failing that, `layout.sidecar_location()`.

The middle step is what keeps a mixed node mixed: a new key joins the sidecar the node actually
has, rather than creating the one its layout would have chosen.

### 4.4 Tolerance

Reading does not fail on a tree that disagrees with its schema. Where both `S.md` and
`S/content.md` exist, intent picks and the loser is reported as a `StrayContent` finding; neither
is deleted. Where a name is found on both edges, the rule's declared edge picks and the loser is a
`StrayChild`; with no rule to break the tie it is an `AmbiguousChild`, which the default policy
rates `Error`.

A metadata file that does not parse is **retained** in a `Malformed` state carrying its raw text
and the parse error, and reported. It never aborts a read, and it is written back verbatim, so a
load/save cycle cannot destroy a file the library could not read.

### 4.5 Metadata is a list of sources

`EntityMeta` is the set of sources observed on disk — front matter, parallel sidecar, inside
sidecar — normalised into `MetaLocation` order (`InHeader` < `ParallelSidecar` < `InsideSidecar`)
so merge precedence is deterministic regardless of probe order.

Sources **merge per key**. Disjoint keys combine; the same key with the same value is not a
conflict; the same key with different values is a `MetadataKeyConflict`, which the default policy
rates `Error`. `location_of(key)` is what routes a write, and it is why nothing moves as a side
effect of setting an unrelated key.

In-header metadata is orthogonal to layout, so it is never "misplaced" — a sidecar can be, and is
reported as `MetadataLocationNonconformance`.

## 5. The shape this exists to preserve

```
book/chapters/000-the-invisible-kitchen.md      the chapter's own content
book/chapters/000-the-invisible-kitchen/        its sections, beside it
    010-what-fermentation-is.md
    review.md
```

A readable spine file with a folder of children next to it. The chapter is `Parallel`; its
children attach on the `Slash` edge. That combination is only expressible because layout and edge
are separate declarations.

## 6. When is there a node here at all?

A node exists if it has content, or metadata, or children, or a directory. An `Inside` node's
directory is its trace even when empty, which is why creating one with nothing to write is
allowed; a `Parallel` node with nothing to write would leave no trace at all, so it is refused.

## 7. Types

A child's type comes from the rule that matched its name. An `Additional` child has no rule and
therefore no declared type: it is `Auto`, and its type must be recorded in its own metadata.

`type` in a node's metadata is checked against what the parent's rule assigned; a disagreement is
a `TypeMismatch` finding, rated `Error` by default.

`ignore` entries are **regexes**, matched the same way `children` rules are. Every regex in a
schema is compiled once when the type is added, so `add_entity_type` fails on an invalid pattern
rather than surprising a later lookup.

## 8. Creating children

`LiveEntity::create_child` takes a **name**. The caller never builds a path.

`ChildBuilder::build` resolves, in order: the type (from `match_child`), the edge, the layout, and
then the paths. Edge resolution follows §4.3's spirit — an existing child of that name keeps its
edge; otherwise, if every existing child of the same rule sits on one edge, the new one joins them;
otherwise the rule's declaration wins, so a split never spreads. Layout is the child type's
declaration, falling back to the parent instance's layout.

`with_edge` and `with_layout` override each aspect independently. They are the escape hatch, not
the ordinary path.

## 9. Findings

Two different things are reported, through one channel.

**Nonconformance** is "this node disagrees with its schema". It has an obvious fix, and a tree
humans edit by hand is full of it, so it defaults to `Warn`.

**Ambiguity** is "this node does not agree with itself". There is no resolution the library can
pick without inventing one, so it defaults to `Error`.

Severity is configurable per kind via `FindingPolicy`; `strict()` makes everything an error and
`silent()` records nothing. `EntityLoader` walks the whole tree, so an `Error` fails the load
up front; `LiveEntity` is lazy, so an `Error` fails only the accessor that produced it —
`metadata()` can fail while `children()` still succeeds.

`Entity.findings` (with `Entity::all_findings()` for the subtree) carries what the eager loader
saw. `LiveEntity::issues()` computes the same for one node at call time, and never fails on a
finding whatever the policy says — reporting is its whole job.

### 9.3 Moving

`move_to` **relocates and does not normalise**. Every file keeps the layout it arrived with, at
the new stem, and dot-descendants are dragged along because their paths derive from the moved
node's. If the new position intends a different layout, that is a finding on the moved node, not
something the move repairs.

Normalisation — `NormaliseSpec`, `plan_normalise`, `MovePlan` — is §9.2–9.4 of the v2 spec and is
not implemented.

---

## Not implemented

- §9.2–9.4: normalisation and move planning.
- D7 (what happens to a stray during normalisation) and D8 (surviving an I/O failure mid-execution)
  are unruled.
- §8.2, the Lua `add_child{}` table-call surface, lives in `inscenerator-booker-agents4`.
