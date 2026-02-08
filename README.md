# InScenerator Entity

`inscenerator-entity` is a Rust library that defines a format for hierarchical entities and provides functions for loading, saving, and eventually manipulating and validating them. It is the core engine for representing complex, structured documents.

## Entity Model

In this library, (almost) everything is an **Entity**. An entity has:
*   A **Type** (string).
*   Optional **Content** (Markdown).
*   Optional **Metadata** (TOML).
*   A **Path** (composed of names joined by `/` or `.`).
*   Optional **Children** (which are also entities).

### Storage Conventions

Entities can be stored on disk using two primary conventions for their content and metadata:

1.  **Inside:** Content is stored at `P/content.md` and metadata at `P/meta.toml`.
2.  **Parallel:** Content is stored at `P.md` and metadata at `P.meta.toml`.

Both forms cannot exist for the same entity simultaneously.

#### Example Layout

```
my-project/
├── 010_chapter-one/             <-- "Inside" structure (directory)
│   ├── content.md               <-- Content for 010_chapter-one
│   ├── 010_first-scene.md       <-- "Parallel" structure (file)
│   ├── 020_second-scene.md
│   └── meta.toml                <-- Metadata for 010_chapter-one
├── 020_chapter-two.md           <-- "Parallel" structure
├── 020_chapter-two.notes.md     <-- "Dot-child" of 020_chapter-two
├── 020_chapter-two.meta.toml    <-- Metadata for 020_chapter-two
└── project.meta.toml            <-- Metadata for the root project (Parallel)
```

In this example:
*   `010_chapter-one` uses the **Inside** structure: its metadata is in `010_chapter-one/meta.toml`.
*   `010_first-scene` uses the **Parallel** structure: its content is in `010_chapter-one/010_first-scene.md`.
*   `020_chapter-two.notes` is a **Dot-child** of `020_chapter-two`.

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

### Hierarchy

Children of an entity can be associated in two ways:
1.  **Slash type:** Children are located in a subdirectory (e.g., `P/childname`).
2.  **Dot type:** Children are located using a dot separator (e.g., `P.childname`).

The **Root** element is special: its path is empty, it must represent a directory, and it can only have "Slash type" children.

## Integration

To use `inscenerator-entity` in your project, add it as a dependency in your `Cargo.toml`.

This library uses `xfs`, a filesystem abstraction crate. This allows you to work with different storage backends (like a real disk via `OsFs` or in-memory for testing via `MockFS`) without changing your logic.

### Defining Entity Types

You can define the structure of your entities programmatically or by loading a schema from a file. This tells the `EntityLoader` what children to expect and their types.

#### Loading Schema from File

A schema can be defined in a `schema.toml` file at the root of your project:

```toml
[Project]
allow_additional = false
[[Project.children]]
name_regex = "^[0-9]+_"
node_type = "Chapter"
required = false
multiple = true

[Chapter]
allow_additional = true
children = []
```

You can then load this schema and the root entity using `load_schema_and_root`:

```rust
use inscenerator_entity::schema::load_schema_and_root;
use xfs::OsFs;
use std::path::Path;

let fs = OsFs {};
let (schema, root) = load_schema_and_root(&fs, Path::new("./my-project")).unwrap();
```

#### Programmatic Definition

Alternatively, you can build the schema manually:

```rust
use inscenerator_entity::entity::{EntityTypeDescription, ChildEntityRules, EntityLoader};
use inscenerator_entity::schema::Schema;

fn setup_loader() -> EntityLoader {
    let mut schema = Schema::new();

    // Create a rule for children with a specific prefix
    let chapter_rule = ChildEntityRules {
        name_regex: "^[0-9]+_".to_string(),
        node_type: "Chapter".to_string(),
        required: false,
        multiple: true,
    };

    schema.add_entity_type(EntityTypeDescription {
        name: "Project".to_string(),
        children: vec![chapter_rule],
        allow_additional: false,
    });

    EntityLoader { schema }
}
```

### Loading and Saving Entities

```rust
use inscenerator_entity::entity::{EntityLoader, EntityWriter, EntityPath};
use xfs::OsFs;
use std::path::Path;
use anyhow::anyhow;

fn main() -> anyhow::Result<()> {
    let mut fs = OsFs {}; // Implementation of Xfs
    let base_path = Path::new("./my-project");
    let loader = setup_loader();
    let writer = EntityWriter {};

    // Loading an entity tree
    let entity = loader
        .try_load_entity(&fs, &base_path, &EntityPath::empty(), "Project")?
        .ok_or_else(|| anyhow!("Project not found"))?;

    println!("Loaded entity: {}", entity.node_type);

    // Saving an entity tree
    writer.write_entity(&mut fs, &base_path, &entity)?;

    Ok(())
}
```

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
