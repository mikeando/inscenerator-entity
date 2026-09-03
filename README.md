# InScenerator Entity

`inscenerator-entity` is a Rust library that defines a format for hierarchical entities and provides functions for loading, saving, and eventually manipulating and validating them. It is the core engine for representing complex, structured documents.

## Entity Model

In this library, (almost) everything is an **Entity**. An entity has:
*   A **Type** (string).
*   Optional **Content** (Markdown).
*   Optional **Metadata** (TOML).
*   A **Path** (composed of names joined by `/` or `.`).
*   Optional **Children** (which are also entities).

### Layout and edge

Where an entity's files land is decided by two declarations, and they are **orthogonal**:

*   The child's **type** declares its **layout** — where its own content and metadata sit,
    relative to its stem `P`:

    | Layout | Content | Metadata |
    | --- | --- | --- |
    | `inside` | `P/content.md` | `P/meta.toml` |
    | `parallel` | `P.md` | `P.meta.toml` |

*   The parent's **child rule** declares the **edge** — how the child's stem is built from
    the parent's:

    | Edge | Stem |
    | --- | --- |
    | `slash` | `disk(parent)/name` |
    | `dot` | `disk(parent).name` |

Keeping them apart is what makes the useful shape below expressible: a chapter that is a
readable `010_chapter-one.md` file (parallel layout) with a `010_chapter-one/` directory of
scenes beside it (slash edge).

A type that declares no `layout` takes the layout of the parent **instance** it was reached
under, so a whole subtree can be switched by changing one type. The **root** is always
`inside`, must be a directory, and has no dot children — there is no filename to prefix.

An entity name may not contain a `.`: `my.file.md` is the entity `my` with a dot-child
`file`.

#### Example Layout

```
my-project/
├── meta.toml                    <-- metadata for the root itself
├── 010_chapter-one.md           <-- the chapter's own content ("parallel" layout)
├── 010_chapter-one.meta.toml    <-- and its metadata
├── 010_chapter-one/             <-- its children, on the "slash" edge
│   ├── 010_first-scene.md
│   └── 020_second-scene.md
├── 020_chapter-two/             <-- a chapter in "inside" layout instead
│   ├── content.md
│   └── meta.toml
└── 020_chapter-two.notes.md     <-- "notes", a child on the "dot" edge
```

The root's metadata is `meta.toml` **inside** the root directory, because the root is always
`inside`. A file named `project.meta.toml` sitting there is not the root's metadata — it is a
metadata-only child entity named `project`.

#### Entity Mapping

The above layout maps to the following entity tree and logical paths:

```
Project (path: "")
├── 010_chapter-one (path: "/010_chapter-one")
│   ├── 010_first-scene (path: "/010_chapter-one/010_first-scene")
│   └── 020_second-scene (path: "/010_chapter-one/020_second-scene")
└── 020_chapter-two (path: "/020_chapter-two")
    └── notes (path: "/020_chapter-two.notes")
```

### Reading is tolerant

A tree that humans edit by hand drifts from its schema. Reading it does not fail: a node
whose files are not where its type intends still loads, and the difference is recorded as a
**finding** rather than an error. `Entity.findings` (and `Entity::all_findings()`) carry what
the eager loader saw; `LiveEntity::issues()` computes the same for one node on demand.

Severity is configurable per finding kind through a `FindingPolicy`. Drift defaults to
`Warn`; ambiguity the library cannot resolve without inventing an answer — a metadata key
with two different values, a child name found on both edges with no rule to break the tie —
defaults to `Error`. `FindingPolicy::strict()` refuses any tree that drifts at all;
`silent()` takes the data and asks no questions.

## Integration

To use `inscenerator-entity` in your project, add it as a dependency in your `Cargo.toml`.

This library uses `inscenerator-xfs`, a filesystem abstraction crate. This allows you to work with different storage backends (like a real disk via `OsFs` or in-memory for testing via `MockFS`) without changing your logic.

### Defining Entity Types

You can define the structure of your entities programmatically or by loading a schema from a file. This tells the `EntityLoader` what children to expect and their types.

#### Loading Schema from File

A schema can be defined in a `schema.toml` file at the root of your project:

```toml
[Project]
allow_additional = false
# Regexes; a child name matching one of these produces no entity at all.
ignore = ['^\.', '^README\.md$']
[[Project.children]]
name_regex = "^[0-9]+_"
node_type = "Chapter"
required = false
multiple = true
edge = "slash"          # where the child attaches. "slash" is the default.

[Chapter]
allow_additional = true
children = []
layout = "parallel"     # where this type's own files live. Omit to inherit.
[[Chapter.children]]
name_regex = "^notes$"
node_type = "Note"
edge = "dot"            # 010_chapter-one.notes, beside the chapter file

[Note]
allow_additional = false
children = []
```

You can then load this schema and the root entity using `load_schema_and_root`:

```rust
use inscenerator_entity::schema::load_schema_and_root;
use inscenerator_xfs::OsFs;
use std::path::Path;

let fs = OsFs {};
let (schema, root) = load_schema_and_root(&fs, Path::new("./my-project")).unwrap();
```

#### Programmatic Definition

Alternatively, you can build the schema manually. `add_entity_type` compiles the type's
regexes, so it fails on an invalid one rather than surprising you at load time:

```rust
use inscenerator_entity::entity::EntityLoader;
use inscenerator_entity::placement::{Edge, Layout};
use inscenerator_entity::schema::{ChildEntityRules, EntityTypeDescription, Schema};

fn setup_loader() -> anyhow::Result<EntityLoader> {
    let mut schema = Schema::new();

    schema.add_entity_type(EntityTypeDescription {
        name: "Project".to_string(),
        children: vec![ChildEntityRules {
            name_regex: "^[0-9]+_".to_string(),
            node_type: "Chapter".to_string(),
            required: false,
            multiple: true,
            edge: Edge::Slash,
        }],
        allow_additional: false,
        ignore: vec![],
        layout: None,
    })?;
    schema.add_entity_type(EntityTypeDescription {
        name: "Chapter".to_string(),
        children: vec![],
        allow_additional: true,
        ignore: vec![],
        layout: Some(Layout::Parallel),
    })?;

    let mut loader = EntityLoader::new();
    loader.schema = schema;
    Ok(loader)
}
```

### Loading and Saving Entities

```rust
use inscenerator_entity::entity::{EntityLoader, EntityWriter};
use inscenerator_xfs::OsFs;
use std::path::Path;
use anyhow::anyhow;

fn main() -> anyhow::Result<()> {
    let mut fs = OsFs {}; // Implementation of Xfs
    let base_path = Path::new("./my-project");
    let loader = setup_loader()?;
    let writer = EntityWriter {};

    // Loading an entity tree. The root is always "inside", so it inherits nothing.
    let entity = loader
        .try_load_root(&fs, &base_path, "Project")?
        .ok_or_else(|| anyhow!("Project not found"))?;

    println!("Loaded entity: {}", entity.node_type);
    for finding in entity.all_findings() {
        println!("  drift: {}", finding.kind);
    }

    // Saving an entity tree
    writer.write_entity(&mut fs, &base_path, &entity)?;

    Ok(())
}
```

### Creating Children (ChildBuilder)

`LiveEntity::create_child` takes a **name** and returns a fluent `ChildBuilder`; call
`.build()` to validate and write to disk. The caller never builds a path: the name is
matched against the parent's rules, and the rule supplies the type and the edge while the
resolved type supplies the layout. Use `.with_type()` only when the schema slot is `"Auto"`
or `allow_additional = true`.

```rust
use inscenerator_entity::entity::Metadata;

// The rule matching "010_chapter-one" says Chapter, on the slash edge; Chapter's type
// says parallel layout. So: 010_chapter-one.md + 010_chapter-one.meta.toml.
let chapter = project
    .create_child("010_chapter-one")
    .with_content("# Chapter One\n\nOnce upon a time...")
    .with_metadata(Metadata { value: toml::from_str("title = \"Chapter One\"")? })
    .build()?;

// The Chapter rule for "notes" declares the dot edge: 010_chapter-one.notes.md
chapter
    .create_child("notes")
    .with_content("Research notes.")
    .build()?;

// Nested children, built in one call
project
    .create_child("020_chapter-two")
    .with_child("010_first-scene", |b| b.with_content("The scene begins..."))
    .build()?;

// An additional slot has no rule and therefore no declared type, so supply one; it is
// written into the child's metadata.
let item = project
    .create_child("epilogue")
    .with_type("Epilogue")
    .with_content("The end.")
    .build()?;
// item.actual_type()? == "Epilogue"
```

Where a new child lands is resolved, not guessed. If existing children of the same rule all
sit on one edge, a new one joins them rather than following the declaration — editing a tree
never fights the convention already established in it. `.with_edge()` and `.with_layout()`
override each aspect independently when you mean to.

See `examples/create_child.rs` for a complete runnable example.

## Development

### GitHub Codespaces / VS Code Dev Containers

This repository includes a [devcontainer configuration](.devcontainer/devcontainer.json) that automatically sets up your development environment with:
- Rust toolchain
- rust-analyzer extension
- TOML language support
- Crates.io helper extension

Simply open in GitHub Codespaces or use "Reopen in Container" in VS Code with the Dev Containers extension.

### Local Development with inscenerator-xfs

If you're developing both `inscenerator-entity` and `inscenerator-xfs` locally, you can override the crates.io dependency with a local path by creating `.cargo/config.toml`:

```toml
[patch.crates-io]
inscenerator-xfs = { path = "../inscenerator-xfs" }
```

This file is gitignored and won't affect other developers or CI/CD.
