/// Demonstrates the ChildBuilder API for creating child entities.
///
/// The point of the v2 API is that the caller names a child and the *schema* decides
/// where it lands: the parent's rule declares the edge, the child's type declares the
/// layout. Nothing here builds a path.
///
/// Run with: cargo run --example create_child
use inscenerator_entity::entity::{EntityPath, Metadata};
use inscenerator_entity::live_entity::LiveEntity;
use inscenerator_entity::placement::{Edge, Layout};
use inscenerator_entity::schema::{ChildEntityRules, EntityTypeDescription, Schema};
use inscenerator_xfs::mockfs::MockFS;
use inscenerator_xfs::Xfs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

fn rule(name_regex: &str, node_type: &str, edge: Edge) -> ChildEntityRules {
    ChildEntityRules {
        name_regex: name_regex.to_string(),
        node_type: node_type.to_string(),
        required: false,
        multiple: true,
        edge,
    }
}

fn print_tree(fs: &dyn Xfs, dir: &Path, indent: usize) {
    let mut entries: Vec<PathBuf> = fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
    entries.sort();
    for p in entries {
        let name = p.file_name().unwrap().to_string_lossy().to_string();
        let is_dir = fs.is_dir(&p);
        println!("{:indent$}{}{}", "", name, if is_dir { "/" } else { "" }, indent = indent);
        if is_dir {
            print_tree(fs, &p, indent + 2);
        }
    }
}

fn main() -> anyhow::Result<()> {
    // Set up an in-memory filesystem and schema.
    let fs = Arc::new(Mutex::new(MockFS::new()));

    let mut schema = Schema::new();
    schema.add_entity_type(EntityTypeDescription {
        name: "Project".to_string(),
        children: vec![rule("^[0-9]+_", "Chapter", Edge::Slash)],
        allow_additional: false,
        // The root is always inside, whatever a type says.
        layout: None,
        ignore: vec![],
    })?;
    schema.add_entity_type(EntityTypeDescription {
        name: "Chapter".to_string(),
        children: vec![
            rule("^[0-9]+_", "Scene", Edge::Slash),
            // A note hangs off the chapter's own filename rather than sitting in its
            // directory, so `010_chapter-one.notes.md` sorts beside the chapter.
            rule("^notes$", "Note", Edge::Dot),
        ],
        allow_additional: true,
        // A readable spine file, with the chapter's directory of scenes beside it (§5).
        layout: Some(Layout::Parallel),
        ignore: vec![],
    })?;
    schema.add_entity_type(EntityTypeDescription {
        name: "Scene".to_string(),
        children: vec![],
        allow_additional: false,
        // Declares nothing, so it inherits Parallel from the chapter it is created under.
        layout: None,
        ignore: vec![],
    })?;
    schema.add_entity_type(EntityTypeDescription {
        name: "Note".to_string(),
        children: vec![],
        allow_additional: false,
        layout: Some(Layout::Parallel),
        ignore: vec![],
    })?;
    schema.add_entity_type(EntityTypeDescription {
        name: "Appendix".to_string(),
        children: vec![],
        allow_additional: false,
        layout: Some(Layout::Inside),
        ignore: vec![],
    })?;
    let schema = Arc::new(schema);

    fs.lock().unwrap().create_dir_all(&PathBuf::from("my-project"))?;
    let project = LiveEntity::new(
        fs.clone(),
        PathBuf::from("my-project"),
        EntityPath::empty(),
        "Project".to_string(),
        schema,
    );

    // 1. The type comes from the rule matching the name: "^[0-9]+_" -> Chapter.
    let chapter = project
        .create_child("010_chapter-one")
        .with_content("# Chapter One\n\nOnce upon a time...")
        .with_metadata(Metadata { value: toml::from_str("title = \"Chapter One\"")? })
        .build()?;
    println!("Created chapter: 010_chapter-one (type={})", chapter.node_type);

    // 2. Nested children, built in one call.
    project
        .create_child("020_chapter-two")
        .with_content("# Chapter Two")
        .with_child("010_first-scene", |b| b.with_content("The scene begins..."))
        .with_child("020_second-scene", |b| b.with_content("The scene continues..."))
        .build()?;
    println!("Created chapter 2 with 2 nested scenes");

    // 3. A dot child. The caller says "notes"; the Chapter rule says that name attaches
    //    on the dot edge.
    chapter.create_child("notes").with_content("Research notes for chapter one.").build()?;
    println!("Created dot child: 010_chapter-one.notes");

    // 4. An `allow_additional` slot has no rule and therefore no declared type, so the
    //    caller supplies one — which is recorded in the child's metadata.
    chapter.create_child("appendix").with_type("Appendix").with_content("The end.").build()?;
    println!("Created appendix (additional slot, type written to metadata)");

    println!("\nmy-project/");
    print_tree(&*fs.lock().unwrap(), Path::new("my-project"), 2);

    Ok(())
}
