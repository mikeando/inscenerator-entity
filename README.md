# InScenerator Entity

`inscenerator-entity` is a Rust library that provides a generic system for parsing and manipulating hierarchical entities stored on a file system. It is the core engine for representing complex, structured documents.

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

### Hierarchy

Children of an entity can be associated in two ways:
1.  **Slash type:** Children are located in a subdirectory (e.g., `P/childname`).
2.  **Dot type:** Children are located using a dot separator (e.g., `P.childname`).

The **Root** element is special: its path is empty, it must represent a directory, and it can only have "Slash type" children.

## Integration

To use `inscenerator-entity` in your project, add it as a dependency in your `Cargo.toml`.

### Defining Entity Types

You must define the structure of your entities using `EntityTypeDescription` and `ChildEntityRules`. This tells the `EntityLoader` what children to expect and their types.

```rust
use inscenerator_entity::entity::{EntityTypeDescription, ChildEntityRules, EntityLoader};

fn setup_loader() -> EntityLoader {
    let mut loader = EntityLoader::new();

    loader.entity_types.insert(
        "Project".to_string(),
        EntityTypeDescription {
            name: "Project".to_string(),
            children: vec![
                ChildEntityRules {
                    name_regex: "^[0-9]+_".to_string(),
                    node_type: "Chapter".to_string(),
                    required: false,
                    multiple: true,
                }
            ],
            allow_additional: false,
        },
    );

    loader.entity_types.insert(
        "Chapter".to_string(),
        EntityTypeDescription {
            name: "Chapter".to_string(),
            children: vec![
                ChildEntityRules {
                    name_regex: "^[0-9]+_".to_string(),
                    node_type: "Scene".to_string(),
                    required: false,
                    multiple: true,
                }
            ],
            allow_additional: false,
        },
    );

    loader.entity_types.insert(
        "Scene".to_string(),
        EntityTypeDescription {
            name: "Scene".to_string(),
            children: vec![],
            allow_additional: true,
        },
    );

    loader
}
```

### Loading Entities

Use the `EntityLoader` to load an entity tree from a given base path.

```rust
use inscenerator_entity::entity::{EntityLoader, EntityPath};
use xfs::OsFs; // Or any other Xfs implementation
use std::path::Path;

fn main() {
    let fs = OsFs {};
    let base_path = Path::new("./my-project");
    let loader = setup_loader();

    match loader.try_load_entity(&fs, &base_path, &EntityPath::empty(), "Project") {
        Ok(Some(entity)) => {
            println!("Loaded entity: {}", entity.node_type);
            for child in &entity.children {
                println!("  Child: {:?}", child.path.local_path());
            }
        }
        Ok(None) => println!("No entity found at root."),
        Err(e) => eprintln!("Error loading: {}", e),
    }
}
```

The [`inscenerator-app`](../inscenerator-app) crate provides a command-line interface for working with InScenerator projects and serves as a primary reference implementation of this library.
