/// Demonstrates the ChildBuilder API for creating child entities.
///
/// Run with: cargo run --example create_child
use inscenerator_entity::entity::{EntityPathEntry, Metadata};
use inscenerator_entity::live_entity::LiveEntity;
use inscenerator_entity::schema::{ChildEntityRules, EntityTypeDescription, Schema};
use inscenerator_xfs::mockfs::MockFS;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

fn main() -> anyhow::Result<()> {
    // Set up an in-memory filesystem and schema
    let fs = Arc::new(Mutex::new(MockFS::new()));

    let mut schema = Schema::new();
    schema.add_entity_type(EntityTypeDescription {
        name: "Project".to_string(),
        children: vec![ChildEntityRules {
            name_regex: "^[0-9]+_".to_string(),
            node_type: "Chapter".to_string(),
            required: false,
            multiple: true,
        }],
        allow_additional: false,
    });
    schema.add_entity_type(EntityTypeDescription {
        name: "Chapter".to_string(),
        children: vec![ChildEntityRules {
            name_regex: ".*".to_string(),
            node_type: "Scene".to_string(),
            required: false,
            multiple: true,
        }],
        allow_additional: true,
    });
    schema.add_entity_type(EntityTypeDescription {
        name: "Scene".to_string(),
        children: vec![],
        allow_additional: false,
    });
    let schema = Arc::new(schema);

    let project = LiveEntity::new(
        fs.clone(),
        PathBuf::from("my-project"),
        inscenerator_entity::entity::EntityPath::empty(),
        "Project".to_string(),
        schema,
    );

    // 1. Simple Slash child — type inferred from schema rule "^[0-9]+_" → "Chapter"
    let chapter = project
        .create_child(EntityPathEntry::Slash("010_chapter-one".to_string()))
        .with_content("# Chapter One\n\nOnce upon a time...")
        .with_metadata_inside(Metadata {
            value: toml::from_str("title = \"Chapter One\"")?,
        })
        .build()?;

    println!("Created chapter: {} (type={})", "010_chapter-one", chapter.node_type);

    // 2. Nested children via with_child
    let _chapter2 = project
        .create_child(EntityPathEntry::Slash("020_chapter-two".to_string()))
        .with_content("# Chapter Two")
        .with_child(
            EntityPathEntry::Slash("010_first-scene".to_string()),
            |b| b.with_content("The scene begins..."),
        )
        .with_child(
            EntityPathEntry::Slash("020_second-scene".to_string()),
            |b| b.with_content("The scene continues..."),
        )
        .build()?;

    println!("Created chapter 2 with 2 nested scenes");

    // 3. Dot child — must have content, metadata, or nested children
    chapter
        .create_child(EntityPathEntry::Dot("notes".to_string()))
        .with_content("Research notes for chapter one.")
        .build()?;

    println!("Created dot child: 010_chapter-one.notes");

    // 4. Auto-type slot — allow_additional=true with with_type() override
    //    (type "Chapter" is written into meta.toml; returned node_type is "Auto")
    let _custom = project
        .create_child(EntityPathEntry::Slash("030_epilogue".to_string()))
        .with_type("Epilogue")
        .with_content("The end.")
        .build()?;

    println!("Created 030_epilogue (Auto slot, actual type=Epilogue)");

    Ok(())
}
