//! Reading a tree that disagrees with its schema.
//!
//! Trees get edited by hand, so drift is normal and reading is tolerant: a node that does
//! not match its type's intent still loads, and the difference is recorded as a `Finding`.
//! How much each kind of finding matters is yours to set, per kind, through a `FindingPolicy`.
//!
//! Run with: cargo run --example findings

use inscenerator_entity::entity::EntityLoader;
use inscenerator_entity::findings::{FindingKindId, FindingPolicy, Severity};
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
name_regex = '^colophon$'
node_type = "Chapter"
edge = "slash"
required = true

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
"#;

fn write(fs: &mut MockFS, path: &str, content: &str) {
    let p = PathBuf::from(path);
    fs.create_dir_all(p.parent().unwrap()).unwrap();
    fs.write(&p, content).unwrap();
}

/// A tree with four separate disagreements in it, all of them things a person could
/// plausibly have done by hand.
fn drifted_fs() -> MockFS {
    let mut fs = MockFS::new();
    write(&mut fs, "book/meta.toml", "type = \"Book\"\n");

    // 1. The chapter's type says parallel, but its content sits inside its directory.
    write(&mut fs, "book/010-chapter/content.md", "# Chapter One\n");

    // 2. ...while its sidecar is where the type says. A mixed node: legal, loads correctly,
    //    stays mixed, and only the half that disagrees is reported.
    write(&mut fs, "book/010-chapter.meta.toml", "word_count = 2400\n");

    // 3. The note's rule says it attaches on the dot edge; this one is on the slash edge.
    write(&mut fs, "book/010-chapter/notes.md", "Ask about the 1976 paper.\n");

    // 4. A sidecar that does not parse.
    write(&mut fs, "book/020-chapter.md", "# Chapter Two\n");
    write(&mut fs, "book/020-chapter.meta.toml", "word_count = = 900\n");

    // ...and the `colophon` rule is `required`, with nothing matching it.
    fs
}

fn main() -> anyhow::Result<()> {
    let fs = drifted_fs();
    let base = Path::new("book");
    let schema = Schema::load_from_str(SCHEMA)?;

    // ---- The default policy: drift is a warning, so the tree loads.
    let mut loader = EntityLoader::new();
    loader.schema = schema.clone();
    let root = loader.try_load_root(&fs, base, "Book")?.expect("the tree is there");

    println!("Loaded under the default policy. Findings:\n");
    for f in root.all_findings() {
        println!("  {:<18} {}", format!("{:?}", f.path.local_path()), first_line(&f.kind.to_string()));
    }

    println!("\n...and the data is all still there:");
    println!("  chapter one content : {:?}", root.children[0].content.content());
    println!("  chapter one metadata: {:?}", root.children[0].metadata.locations());
    println!(
        "  chapter two's malformed sidecar is kept, not dropped: {:?}",
        root.children[1].metadata.malformed().iter().map(|s| s.error()).collect::<Vec<_>>()
    );

    // ---- strict(): every kind is an error, so the load fails on the first one.
    let mut strict = EntityLoader::new().with_policy(FindingPolicy::strict());
    strict.schema = schema.clone();
    match strict.try_load_root(&fs, base, "Book") {
        Ok(_) => println!("\nstrict(): loaded (unexpected)"),
        Err(e) => println!("\nstrict(): refused the tree — {}", first_line(&e.to_string())),
    }

    // ---- silent(): nothing is recorded at all.
    let mut quiet = EntityLoader::new().with_policy(FindingPolicy::silent());
    quiet.schema = schema.clone();
    let quiet_root = quiet.try_load_root(&fs, base, "Book")?.unwrap();
    println!("silent(): loaded with {} findings", quiet_root.all_findings().len());

    // ---- One kind at a time. Most useful in a linter: promote what you care about.
    let policy = FindingPolicy::default()
        .with(FindingKindId::MalformedMetadata, Severity::Error)
        .with(FindingKindId::MissingRequiredChild, Severity::Ignore);
    let mut tuned = EntityLoader::new().with_policy(policy.clone());
    tuned.schema = schema.clone();
    match tuned.try_load_root(&fs, base, "Book") {
        Ok(_) => println!("tuned   : loaded (unexpected)"),
        Err(e) => println!("tuned   : refused on the malformed sidecar — {}", first_line(&e.to_string())),
    }

    // ---- The lazy side. `issues()` reports one node without reading the tree, and never
    // fails on a finding: reporting is its whole job.
    let live = LiveEntity::load_from_root_with(
        Arc::new(Mutex::new(drifted_fs())),
        PathBuf::from("book"),
        Arc::new(schema),
    )?;
    let chapter = live.child("010-chapter")?;
    println!("\nLiveEntity::issues() on 010-chapter:");
    for f in chapter.issues()? {
        println!("  {}", f.kind);
    }

    // A policy that rates something an `Error` fails only the accessor that produced it.
    // The chapter's content is fine, so `content()` succeeds either way.
    // Note the policy is applied to the chapter handle, not the root: under strict() the
    // root's own findings (the missing `colophon`) would fail the child lookup itself.
    let strict_chapter = live.child("020-chapter")?.with_policy(FindingPolicy::strict());
    println!("\nUnder strict(), on the chapter with the malformed sidecar:");
    println!("  content()  -> {:?}", strict_chapter.content()?.content());
    println!("  metadata() -> {:?}", strict_chapter.metadata().map(|_| ()).map_err(|e| first_line(&e.to_string())));

    Ok(())
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").to_string()
}
