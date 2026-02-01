# InScenerator Entity

`inscenerator-entity` is a Rust library that provides the core logic for parsing and manipulating novel manuscripts written in Markdown. It is the engine that powers `inscenerator-app`.

## Project Structure

An InScenerator project is a directory that contains a series of chapter directories. The project may also contain an optional `notes` directory for storing project-level notes.

```
my-novel/
├── 010_chapter-one/
│   ├── 010_first-scene.md
│   ├── 020_second-scene.md
│   └── meta.toml
├── 020_chapter-two/
│   ├── 010_another-scene.md
│   └── meta.toml
└── notes/
    └── research.md
```

### Chapters

Chapters are directories with a specific naming convention: `<number>_<name>`.

*   `<number>`: A three-digit number that determines the order of the chapters.
*   `<name>`: A descriptive name for the chapter.

Each chapter directory can contain an optional `meta.toml` file for chapter-specific metadata.

### Scenes

Scenes are Markdown files within a chapter directory, and they also follow a specific naming convention: `<number>_<name>.md`.

*   `<number>`: A three-digit number that determines the order of the scenes within the chapter.
*   `<name>`: A descriptive name for the scene.

## Document Format

Each scene file is a Markdown document with five sections, separated by `---`:

1.  **Metadata:** A TOML block containing metadata for the scene.
2.  **Summary:** A brief summary of the scene.
3.  **Content:** The main content of the scene.
4.  **Issues:** A section for noting any issues or things to fix.
5.  **Notes:** A section for any other notes related to the scene.

The `Issues` and `Notes` sections are free-form Markdown. They do not have a specific structure on disk beyond being text blocks within the scene file.

### Scene Metadata

The metadata section is a TOML block at the beginning of the file:

```toml
name = "The First Scene"
tags = ["#status-draft", "#pov-character-name"]
```

*   `name`: The title of the scene.
*   `tags`: A list of tags for the scene.

### Chapter Metadata

The `meta.toml` file in a chapter directory can contain the following:

```toml
title = "The First Chapter"
is_chapter = true
```

*   `title`: The title of the chapter.
*   `is_chapter`: A boolean indicating whether this is a chapter (defaults to `true`).

## Integration

To use `inscenerator-core` in your own project, you can add it as a dependency in your `Cargo.toml` file. The main entry point for parsing a project is the `Project::load` function.

Here's a basic example of how to load a project and access scene data:

```rust
use inscenerator_core::structure::Project;
use xfs::OsFs;
use std::path::Path;

fn main() {
    let fs = OsFs {};
    let project_path = Path::new("./my-novel");
    match Project::load(&fs, &project_path) {
        Ok(project) => {
            println!("Successfully loaded project.");
            // Accessing scene details from the first scene of the first chapter
            if let Some(first_chapter) = project.chapters.first() {
                if let Some(first_scene) = first_chapter.scenes.first() {
                    println!("--- First Scene Details ---");
                    println!("Path: {}", first_scene.local_path().display());
                    println!("Issues:\n{}", first_scene.issues);
                    println!("Notes:\n{}", first_scene.notes);
                }
            }
        }
        Err(e) => {
            eprintln!("Failed to load project: {}", e);
        }
    }
}
```

The [`inscenerator-app`](../inscenerator-app) crate provides a command-line interface for working with InScenerator projects and serves as the primary reference implementation.
