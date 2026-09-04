//! Editing one node at a time with `LiveEntity`, and where each write lands.
//!
//! `LiveEntity` is a lazy handle: it holds a path and a schema, and touches disk only when
//! you ask it something. Writes route to whatever the node has **already established** —
//! only a node with nothing there follows its type's intent. That is what lets a hand-edited
//! tree stay the shape its author left it in.
//!
//! Run with: cargo run --example live_editing

use inscenerator_entity::live_entity::LiveEntity;
use inscenerator_entity::placement::MetaLocation;
use inscenerator_entity::schema::Schema;
use inscenerator_xfs::mockfs::MockFS;
use inscenerator_xfs::{Xfs, XfsReadOnly};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

const SCHEMA: &str = r#"
[Book]
allow_additional = false
ignore = ['^schema\.toml$']

[[Book.children]]
name_regex = '^\d{3}-'
node_type = "Chapter"
edge = "slash"
multiple = true

[Chapter]
allow_additional = false
layout = "parallel"

[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"
multiple = true

[Section]
allow_additional = false
# No layout declared, so a section inherits the chapter's.
"#;

fn write(fs: &mut MockFS, path: &str, content: &str) {
    let p = PathBuf::from(path);
    fs.create_dir_all(p.parent().unwrap()).unwrap();
    fs.write(&p, content).unwrap();
}

fn show(fs: &Arc<Mutex<MockFS>>, path: &str) {
    let fs = fs.lock().unwrap();
    let p = PathBuf::from(path);
    if !fs.is_file(&p) {
        println!("  {:<40} (does not exist)", path);
        return;
    }
    let text = fs.read_to_string(&p).unwrap();
    println!("  {:<40} {:?}", path, text);
}

fn main() -> anyhow::Result<()> {
    let mut raw = MockFS::new();
    write(&mut raw, "book/schema.toml", SCHEMA);
    write(&mut raw, "book/meta.toml", "type = \"Book\"\n");
    // A chapter whose metadata is split: `status` in the content file's front matter,
    // `word_count` in a sidecar. Both are legal, and both are common in a hand-edited tree.
    write(
        &mut raw,
        "book/010-chapter.md",
        "```toml\nstatus = \"draft\"\n```\n\n# Chapter One\n",
    );
    write(&mut raw, "book/010-chapter.meta.toml", "word_count = 2400\n");

    let fs = Arc::new(Mutex::new(raw));
    let schema = Arc::new(Schema::load_from_str(SCHEMA)?);
    let book = LiveEntity::load_from_root_with(fs.clone(), PathBuf::from("book"), schema)?;

    // ---- Reading is lazy: nothing was read until now.
    let chapter = book.child("010-chapter")?;
    println!("chapter type   : {}", chapter.actual_type()?);
    println!("intended layout: {:?}", chapter.intended_layout()?);
    println!("observed       : {:?}", chapter.observed()?);
    println!("content        : {:?}", chapter.content()?.content());

    let meta = chapter.metadata()?;
    println!("\nMetadata is two sources, merged per key:");
    for s in meta.sources() {
        println!("  {}", s.describe());
    }
    println!("  status     lives in {:?}", meta.location_of("status"));
    println!("  word_count lives in {:?}", meta.location_of("word_count"));

    // ---- Per-key writes go to the source that already holds the key. Nothing moves.
    chapter.set_meta_key("word_count", toml::Value::Integer(2600))?;
    chapter.set_meta_key("status", toml::Value::String("revised".into()))?;
    println!("\nAfter set_meta_key on each — each key stayed where it was:");
    show(&fs, "book/010-chapter.md");
    show(&fs, "book/010-chapter.meta.toml");

    // ---- A key with no home yet falls back to the layout's sidecar.
    chapter.set_meta_key("reviewer", toml::Value::String("mm".into()))?;
    println!("\nA new key has no source of its own, so it joins the intended sidecar:");
    show(&fs, "book/010-chapter.meta.toml");

    // ---- Removing a key takes it out of wherever it lives, and leaves the rest alone.
    chapter.remove_meta_key("status")?;
    println!("\nAfter remove_meta_key(\"status\") — the front matter block is now empty:");
    show(&fs, "book/010-chapter.md");

    // ---- Content writes go where the content already is.
    //
    // The chapter's type declares `parallel`, and its content file is already 010-chapter.md,
    // so this rewrites that file rather than creating 010-chapter/content.md.
    chapter.set_content("# Chapter One\n\nRewritten.\n")?;
    println!("\nAfter set_content — the front matter that was there is preserved:");
    show(&fs, "book/010-chapter.md");

    // ---- Replacing a whole source, explicitly.
    chapter.set_metadata_at(
        MetaLocation::ParallelSidecar,
        inscenerator_entity::entity::Metadata {
            value: toml::from_str("word_count = 2600\nreviewer = \"mm\"\napproved = true")?,
        },
    )?;
    println!("\nAfter set_metadata_at(ParallelSidecar, ...) — that source, replaced whole:");
    show(&fs, "book/010-chapter.meta.toml");

    // ---- Creating a section under the chapter. The schema decides where it goes.
    chapter.create_child("010-first-section").with_content("Section body.\n").build()?;
    println!("\nA new section, placed by the schema:");
    show(&fs, "book/010-chapter/010-first-section.md");

    println!("\nChildren of the chapter: {:?}", names(&chapter)?);

    // `issues()` reports what this node looks like right now. It never fails on a finding
    // whatever the policy says — see the `findings` example for the policy that does.
    // The empty front-matter block left behind above is still a metadata source, so the
    // chapter's metadata is still (reported as) split across two of them.
    println!("Issues on the chapter:");
    for f in chapter.issues()? {
        println!("  {}", f.kind);
    }

    // ---- clear_metadata removes every source, header included.
    let section = chapter.child("010-first-section")?;
    section.set_meta_key("draft", toml::Value::Boolean(true))?;
    section.clear_metadata()?;
    println!("\nAfter clear_metadata on the section:");
    show(&fs, "book/010-chapter/010-first-section.meta.toml");

    Ok(())
}

fn names(e: &LiveEntity) -> anyhow::Result<Vec<String>> {
    Ok(e.children()?
        .iter()
        .map(|c| c.path().last_name().unwrap_or("<root>").to_string())
        .collect())
}
