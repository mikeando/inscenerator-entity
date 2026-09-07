//! Reading a whole tree eagerly with `EntityLoader`, and writing it back with `EntityWriter`.
//!
//! `EntityLoader` walks the tree once and hands back an `Entity` you can traverse in memory:
//! the type each node resolved to, the layout it was read under, its content, its metadata,
//! and any drift found along the way. `EntityWriter` is the inverse.
//!
//! Use the eager reader when you want the whole tree; use `LiveEntity` (see `live_editing.rs`)
//! when you want to touch one node without reading everything.
//!
//! Run with: cargo run --example load_and_walk

use inscenerator_entity::entity::{Entity, EntityContent, EntityLoader, EntityWriter};
use inscenerator_entity::schema::Schema;
use inscenerator_xfs::mockfs::MockFS;
use inscenerator_xfs::Xfs;
use std::path::{Path, PathBuf};

/// The schema, exactly as it would sit in `book/schema.toml`.
const SCHEMA: &str = r#"
[Book]
allow_additional = false
# Files a tool drops in the tree, which are not entities.
ignore = ['^\.', '^schema\.toml$']

[[Book.children]]
name_regex = '^\d{3}-'
node_type = "Chapter"
edge = "slash"
multiple = true

[Chapter]
allow_additional = false
# A readable spine file, with the chapter's directory of sections beside it.
layout = "parallel"

[[Chapter.children]]
name_regex = '^\d{3}-'
node_type = "Section"
edge = "slash"
multiple = true

[[Chapter.children]]
# A note hangs off the chapter's own filename: book/000-....notes.md
name_regex = '^notes$'
node_type = "Note"
edge = "dot"

[Section]
allow_additional = false
# Declares no layout, so it inherits Parallel from the chapter above it.

[Note]
allow_additional = false
layout = "parallel"
"#;

fn write(fs: &mut MockFS, path: &str, content: &str) {
    let p = PathBuf::from(path);
    fs.create_dir_all(p.parent().unwrap()).unwrap();
    fs.write(&p, content).unwrap();
}

/// A tree as a hand-edited author would leave it — including a stray `.DS_Store`.
fn seeded_fs() -> MockFS {
    let mut fs = MockFS::new();
    write(&mut fs, "book/schema.toml", SCHEMA);
    write(&mut fs, "book/meta.toml", "type = \"Book\"\ntitle = \"The Invisible Kitchen\"\n");
    write(&mut fs, "book/content.md", "A book about fermentation.\n");
    write(
        &mut fs,
        "book/000-the-invisible-kitchen.md",
        // Front matter is a fenced TOML block at the top of the content file.
        "```toml\nword_count = 2400\nstatus = \"draft\"\n```\n\n# The Invisible Kitchen\n",
    );
    write(&mut fs, "book/000-the-invisible-kitchen/010-what-fermentation-is.md", "Section body.\n");
    write(&mut fs, "book/000-the-invisible-kitchen/020-a-short-history.md", "Another section.\n");
    write(&mut fs, "book/000-the-invisible-kitchen.notes.md", "Ask about the 1976 paper.\n");
    write(&mut fs, "book/.DS_Store", "");
    fs
}

fn walk(e: &Entity, depth: usize) {
    let name = e.path.last_name().unwrap_or("<root>");
    let words = e.content.content().map_or(0, |c| c.split_whitespace().count());
    println!(
        "{:indent$}{:<32} type={:<8} layout={:?}  {} words",
        "",
        name,
        e.node_type,
        e.layout,
        words,
        indent = depth * 2
    );
    if let Some(m) = e.metadata.merged().unwrap() {
        let keys: Vec<&str> = m.value.as_table().map_or(vec![], |t| t.keys().map(|k| k.as_str()).collect());
        if !keys.is_empty() {
            println!(
                "{:indent$}  metadata {:?} from {:?}",
                "",
                keys,
                e.metadata.locations(),
                indent = depth * 2
            );
        }
    }
    for c in &e.children {
        walk(c, depth + 1);
    }
}

fn main() -> anyhow::Result<()> {
    let fs = seeded_fs();
    let base = Path::new("book");

    // ---- Read.
    let mut loader = EntityLoader::new();
    loader.schema = Schema::load_from_file(&fs, &base.join("schema.toml"))?;
    let root = loader
        .try_load_root(&fs, base, "Book")?
        .expect("book/ has content, metadata and children, so there is a node there");

    println!("Loaded tree:\n");
    walk(&root, 0);

    // The `ignore` regexes kept .DS_Store and schema.toml out of the tree entirely, so
    // there is nothing to report about them.
    println!("\nFindings: {:?}", root.all_findings());

    // ---- Reach into it.
    let chapter = &root.children[0];
    let meta = chapter.metadata.merged()?.expect("the chapter has front matter");
    println!("\nThe chapter's metadata, read from {:?}:", chapter.metadata.locations());
    for line in toml::to_string(&meta.value)?.lines() {
        println!("  {}", line);
    }

    // ---- Write it back somewhere else.
    //
    // The writer puts each node's files where that node's own `EntityContent` and metadata
    // sources say they are, so a round trip is byte-stable: nothing is normalised on save.
    let mut out = MockFS::new();
    out.create_dir_all(Path::new("copy"))?;
    let mut copy = root.clone();
    copy.content = EntityContent::inside("A book about fermentation. (revised)\n");
    EntityWriter {}.write_entity(&mut out, Path::new("copy"), &copy)?;

    println!("\nWritten back to copy/:\n");
    let mut files = vec![];
    collect(&out, &PathBuf::from("copy"), &mut files);
    for f in files {
        println!("  {}", f);
    }

    Ok(())
}

fn collect(fs: &dyn Xfs, dir: &Path, out: &mut Vec<String>) {
    let mut entries: Vec<PathBuf> = fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
    entries.sort();
    for p in entries {
        if fs.is_dir(&p) {
            collect(fs, &p, out);
        } else {
            out.push(p.to_string_lossy().to_string());
        }
    }
}
