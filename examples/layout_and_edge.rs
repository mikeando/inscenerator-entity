//! Layout and edge are two independent declarations.
//!
//! The **parent's rule** says how a child attaches — `slash` (`parent/name`) or `dot`
//! (`parent.name`). The **child's type** says where the child's own files go — `parallel`
//! (`S.md` + `S.meta.toml`) or `inside` (`S/content.md` + `S/meta.toml`).
//!
//! Nothing infers one from the other, so all four combinations are reachable. This example
//! builds one child of each, and then shows a type that declares no layout at all taking
//! the layout of whichever parent instance it is created under.
//!
//! Run with: cargo run --example layout_and_edge

use inscenerator_entity::entity::EntityPath;
use inscenerator_entity::live_entity::LiveEntity;
use inscenerator_entity::placement::{Edge, Layout};
use inscenerator_entity::schema::{ChildEntityRules, EntityTypeDescription, Schema};
use inscenerator_xfs::mockfs::MockFS;
use inscenerator_xfs::{Xfs, XfsReadOnly};
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

fn leaf(name: &str, layout: Option<Layout>) -> EntityTypeDescription {
    EntityTypeDescription {
        name: name.to_string(),
        children: vec![],
        allow_additional: false,
        layout,
        ignore: vec![],
    }
}

/// Every file under `dir`, sorted, relative to nothing — just the whole tree, flat.
fn all_files(fs: &dyn Xfs, dir: &Path, out: &mut Vec<String>) {
    let mut entries: Vec<PathBuf> = fs.read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
    entries.sort();
    for p in entries {
        if fs.is_dir(&p) {
            all_files(fs, &p, out);
        } else {
            out.push(p.to_string_lossy().to_string());
        }
    }
}

fn main() -> anyhow::Result<()> {
    let mut schema = Schema::new();

    // The root. Always `inside`, whatever a type says, so it declares nothing.
    schema.add_entity_type(EntityTypeDescription {
        name: "Book".to_string(),
        children: vec![
            rule(r"^\d{3}-", "Chapter", Edge::Slash),
            rule("^appendix$", "Appendix", Edge::Slash),
        ],
        allow_additional: false,
        layout: None,
        ignore: vec![],
    })?;

    // One rule per combination. The rule picks the edge; the type it names picks the layout.
    schema.add_entity_type(EntityTypeDescription {
        name: "Chapter".to_string(),
        children: vec![
            rule("^s-", "ParallelLeaf", Edge::Slash),
            rule("^i-", "InsideLeaf", Edge::Slash),
            rule("^notes$", "ParallelLeaf", Edge::Dot),
            rule("^draft$", "InsideLeaf", Edge::Dot),
            // This one declares no layout at all — see the second half of the example.
            rule("^inherits$", "InheritingLeaf", Edge::Slash),
        ],
        allow_additional: false,
        // A readable spine file, with the chapter's directory of children beside it.
        layout: Some(Layout::Parallel),
        ignore: vec![],
    })?;
    // The same `InheritingLeaf` rule, under a parent that is `inside` rather than parallel.
    schema.add_entity_type(EntityTypeDescription {
        name: "Appendix".to_string(),
        children: vec![rule("^inherits$", "InheritingLeaf", Edge::Slash)],
        allow_additional: false,
        layout: Some(Layout::Inside),
        ignore: vec![],
    })?;
    schema.add_entity_type(leaf("ParallelLeaf", Some(Layout::Parallel)))?;
    schema.add_entity_type(leaf("InsideLeaf", Some(Layout::Inside)))?;
    schema.add_entity_type(leaf("InheritingLeaf", None))?;
    let schema = Arc::new(schema);

    let fs = Arc::new(Mutex::new(MockFS::new()));
    fs.lock().unwrap().create_dir_all(&PathBuf::from("book"))?;
    let book = LiveEntity::new(
        fs.clone(),
        PathBuf::from("book"),
        EntityPath::empty(),
        "Book".to_string(),
        schema.clone(),
    );

    let chapter = book.create_child("010-chapter").with_content("# Chapter").build()?;
    for name in ["s-one", "i-one", "notes", "draft"] {
        chapter.create_child(name).with_content("body").build()?;
    }

    println!("The four combinations, from four declarations:\n");
    println!("  {:<12} {:<10} files", "edge", "layout");
    println!("  {:<12} {:<10} -----", "----", "------");
    println!("  {:<12} {:<10} book/010-chapter/s-one.md", "slash", "parallel");
    println!("  {:<12} {:<10} book/010-chapter/i-one/content.md", "slash", "inside");
    println!("  {:<12} {:<10} book/010-chapter.notes.md", "dot", "parallel");
    println!("  {:<12} {:<10} book/010-chapter.draft/content.md", "dot", "inside");

    println!("\nWhat was actually written:\n");
    let mut files = vec![];
    all_files(&*fs.lock().unwrap(), &PathBuf::from("book"), &mut files);
    for f in &files {
        println!("  {}", f);
    }

    // The chapter's spine file sits beside its directory of children. Deriving layout from
    // the edge would force one of the two shapes below instead, and this is the shape most
    // trees actually want: a readable filename you can open, with the children next to it.
    let exists = |p: &str| fs.lock().unwrap().is_file(&PathBuf::from(p));
    assert!(exists("book/010-chapter.md"), "the readable spine file");
    assert!(exists("book/010-chapter/s-one.md"), "with children beside it");
    assert!(!exists("book/010-chapter/content.md"));
    assert!(!exists("book/010-chapter.s-one.md"));

    // ---- Layout is inherited by *instance*, not by type.
    //
    // `InheritingLeaf` declares no layout, so it takes the layout of the parent instance it
    // is created under. The same type is therefore parallel in one part of the tree and
    // inside in another.
    chapter.create_child("inherits").with_content("body").build()?;
    let appendix = book.create_child("appendix").build()?;
    appendix.create_child("inherits").with_content("body").build()?;

    println!("\nInheritingLeaf declares no layout, so it takes its parent instance's:\n");
    println!("  under 010-chapter (parallel) -> book/010-chapter/inherits.md");
    println!("  under appendix    (inside)   -> book/appendix/inherits/content.md");
    assert!(exists("book/010-chapter/inherits.md"));
    assert!(exists("book/appendix/inherits/content.md"));

    // ---- Overriding, one aspect at a time.
    //
    // The schema declaring these things is the point; `with_edge` and `with_layout` are the
    // escape hatch. Because the two are resolved separately, they override separately.
    chapter
        .create_child("s-two")
        .with_layout(Layout::Inside)
        .with_content("body")
        .build()?;
    println!("\nwith_layout(Inside) on one child of the `^s-` rule:");
    println!("  book/010-chapter/s-two/content.md   (still on the rule's slash edge)");
    assert!(exists("book/010-chapter/s-two/content.md"));

    Ok(())
}
