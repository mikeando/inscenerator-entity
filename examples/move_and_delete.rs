//! Moving and deleting nodes.
//!
//! `move_to` **relocates and does not normalise**: every file keeps the layout it arrived
//! with, at the new stem. Descendants attached on the dot edge come along, because their
//! paths derive from the moved node's. If the node's new position intends a different
//! layout, that is a finding on the moved node, not something the move repairs.
//!
//! `delete` removes whatever the node has — both content locations and both sidecars —
//! because deleting removes the node, not one shape of it.
//!
//! Run with: cargo run --example move_and_delete

use inscenerator_entity::entity::EntityPath;
use inscenerator_entity::live_entity::LiveEntity;
use inscenerator_entity::schema::Schema;
use inscenerator_xfs::mockfs::MockFS;
use inscenerator_xfs::Xfs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

const SCHEMA: &str = r#"
[Book]
allow_additional = false

[[Book.children]]
name_regex = '^\d{3}-'
node_type = "Chapter"
edge = "slash"
multiple = true

[[Book.children]]
name_regex = '^archive$'
node_type = "Archive"
edge = "slash"

[Chapter]
allow_additional = false
layout = "parallel"

[[Chapter.children]]
name_regex = '^notes$'
node_type = "Note"
edge = "dot"

[Note]
allow_additional = false
layout = "parallel"

[[Note.children]]
name_regex = '^draft$'
node_type = "Note"
edge = "dot"

# An archived chapter is `inside` — a directory with its content in it.
[Archive]
allow_additional = false
layout = "inside"

[[Archive.children]]
name_regex = '^\d{3}-'
node_type = "ArchivedChapter"
edge = "slash"
multiple = true

[ArchivedChapter]
allow_additional = false
layout = "inside"

[[ArchivedChapter.children]]
name_regex = '^notes$'
node_type = "Note"
edge = "dot"
"#;

fn write(fs: &mut MockFS, path: &str, content: &str) {
    let p = PathBuf::from(path);
    fs.create_dir_all(p.parent().unwrap()).unwrap();
    fs.write(&p, content).unwrap();
}

fn tree(fs: &Arc<Mutex<MockFS>>) -> Vec<String> {
    fn walk(fs: &dyn Xfs, dir: &Path, out: &mut Vec<String>) {
        let mut entries: Vec<PathBuf> =
            fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
        entries.sort();
        for p in entries {
            if fs.is_dir(&p) {
                walk(fs, &p, out);
            } else {
                out.push(p.to_string_lossy().to_string());
            }
        }
    }
    let mut out = vec![];
    walk(&*fs.lock().unwrap(), &PathBuf::from("book"), &mut out);
    out
}

fn print_tree(label: &str, fs: &Arc<Mutex<MockFS>>) {
    println!("\n{}", label);
    for f in tree(fs) {
        println!("  {}", f);
    }
}

fn main() -> anyhow::Result<()> {
    let mut raw = MockFS::new();
    write(&mut raw, "book/meta.toml", "type = \"Book\"\n");
    write(&mut raw, "book/archive/meta.toml", "type = \"Archive\"\n");
    write(&mut raw, "book/010-chapter.md", "# Chapter One\n");
    write(&mut raw, "book/010-chapter.meta.toml", "word_count = 2400\n");
    // A dot-child of the chapter, with its own dot-child beneath it. Each has its own
    // appended sidecar — 010-chapter.notes.meta.toml is *not* the chapter's file.
    write(&mut raw, "book/010-chapter.notes.md", "Ask about the 1976 paper.\n");
    write(&mut raw, "book/010-chapter.notes.meta.toml", "author = \"mm\"\n");
    write(&mut raw, "book/010-chapter.notes.draft.md", "Earlier version.\n");

    let fs = Arc::new(Mutex::new(raw));
    let schema = Arc::new(Schema::load_from_str(SCHEMA)?);
    let book = LiveEntity::load_from_root_with(fs.clone(), PathBuf::from("book"), schema)?;

    print_tree("Before:", &fs);

    // ---- Rename in place. The dot-descendants come with it.
    let mut chapter = book.child("010-chapter")?;
    chapter.move_to(EntityPath::empty().extend_slash("020-chapter"))?;
    print_tree("After renaming 010-chapter -> 020-chapter:", &fs);
    println!("  (the notes and their draft moved too — their paths are built from the chapter's)");

    // ---- Move it somewhere that intends a different layout.
    //
    // `Archive` is `inside`, so a chapter filed there would conform as
    // archive/020-chapter/content.md. The move does not rewrite it: the files keep the
    // parallel shape they had, and the disagreement becomes a finding.
    let mut chapter = book.child("020-chapter")?;
    chapter.move_to(EntityPath::empty().extend_slash("archive").extend_slash("020-chapter"))?;
    print_tree("After moving it under archive/ (which intends `inside`):", &fs);

    // Re-fetch through the new parent: a moved handle's inherited layout is stale if the
    // node changed parents, and `issues()` is computed against what the handle knows.
    let archived = book.child("archive")?.child("020-chapter")?;
    println!("\nissues() on the moved node:");
    for f in archived.issues()? {
        println!("  {}", f.kind);
    }
    println!("  (normalising it — actually moving the files — is a separate operation)");

    // ---- Deleting.
    //
    // `delete(false)` refuses a node with children rather than orphaning them.
    match archived.delete(false) {
        Ok(()) => println!("\ndelete(false) removed it"),
        Err(e) => println!("\ndelete(false) refused: {}", e),
    }
    archived.delete(true)?;
    print_tree("After delete(true):", &fs);
    println!("  (both content locations and both sidecars are removed, whichever the node had)");

    Ok(())
}
