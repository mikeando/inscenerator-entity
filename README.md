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
├── 010_chapter-one/        <-- "Inside" structure (directory)
│   ├── 010_first-scene.md  <-- "Parallel" structure (file)
│   ├── 020_second-scene.md
│   └── meta.toml           <-- Metadata for 010_chapter-one
├── 020_chapter-two.md      <-- "Parallel" structure
└── project.meta.toml       <-- Metadata for the root project (Parallel)
```

In this example:
*   `010_chapter-one` uses the **Inside** structure: its metadata is in `010_chapter-one/meta.toml`.
*   `010_first-scene` uses the **Parallel** structure: its content is in `010_chapter-one/010_first-scene.md`.

### Hierarchy

Children of an entity can be associated in two ways:
1.  **Slash type:** Children are located in a subdirectory (e.g., `P/childname`).
2.  **Dot type:** Children are located using a dot separator (e.g., `P.childname`).

The **Root** element is special: its path is empty, it must represent a directory, and it can only have "Slash type" children.

## Integration

To use `inscenerator-entity` in your project, add it as a dependency in your `Cargo.toml`.

This library uses `xfs`, a filesystem abstraction crate. This allows you to work with different storage backends (like a real disk via `OsFs` or in-memory for testing via `MockFS`) without changing your logic.

### Defining Entity Types

You must define the structure of your entities using `EntityTypeDescription` and `ChildEntityRules`. This tells the `EntityLoader` what children to expect and their types.

```rust
use inscenerator_entity::entity::{EntityTypeDescription, ChildEntityRules, EntityLoader};

fn setup_loader() -> EntityLoader {
    let mut loader = EntityLoader::new();

    fn child_entity(node_type: &str) -> ChildEntityRules {
        ChildEntityRules {
            name_regex: "^[0-9]+_".to_string(),
            node_type: node_type.to_string(),
            required: false,
            multiple: true,
        }
    }

    fn add_entity(loader: &mut EntityLoader, name: &str, children: &[ChildEntityRules]) {
        loader.entity_types.insert(
            name.to_string(),
            EntityTypeDescription {
                name: name.to_string(),
                children: children.to_vec(),
                allow_additional: false,
            },
        );
    }

    add_entity(&mut loader, "Project", &[child_entity("Chapter")]);
    add_entity(&mut loader, "Chapter", &[child_entity("Scene")]);
    add_entity(&mut loader, "Scene", &[]);

    loader
}
```

### Loading and Saving Entities

```rust
use inscenerator_entity::entity::{EntityLoader, EntityWriter, EntityPath};
use xfs::OsFs;
use std::path::Path;

fn main() -> anyhow::Result<()> {
    let mut fs = OsFs {}; // Implementation of Xfs
    let base_path = Path::new("./my-project");
    let loader = setup_loader();

    // Loading an entity tree
    if let Some(entity) = loader.try_load_entity(&fs, &base_path, &EntityPath::empty(), "Project")? {
        println!("Loaded entity: {}", entity.node_type);

        // Saving an entity tree
        let writer = EntityWriter {};
        writer.write_entity(&mut fs, &base_path, &entity)?;
    }

    Ok(())
}
```
