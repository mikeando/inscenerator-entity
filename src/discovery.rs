//! Child discovery — the single resolver both readers call. §3.1, §4.2, §4.4.
//!
//! Two passes look for children. The **dot pass** scans the parent directory for entries
//! prefixed `stem.`; the **slash pass** scans the stem directory itself. Each yields
//! *names*, not paths: a name may be seen by both passes, and committing to an edge
//! inside a pass would make that state unrepresentable.
//!
//! [`resolve_children`] merges the two, consults the schema once per name, and produces a
//! [`ResolvedChild`] per *(edge, name)* — which is the key that identifies a child among
//! its siblings. `name` alone is not: two children may share a name across the edges, and
//! both are real.
//!
//! A name is asked three independent questions, each with its own finding:
//!
//! | Question | Finding | Example on disk |
//! | --- | --- | --- |
//! | Does the schema allow a child by this name at all? | [`FindingKind::UnexpectedChild`] | `stray` under `allow_additional = false` |
//! | Do two children share this name across edges? | [`FindingKind::DuplicateChildName`] | `ch1.notes.md` **and** `ch1/notes.md` |
//! | Is this one node with two content files? | `StrayContent` — the loader's question, not this module's | `ch1.md` **and** `ch1/content.md` |
//!
//! The first is about the schema, the second about two entities, the third about one
//! entity. Only the third discards anything, which is why only it needs intent to pick.
//!
//! `UnexpectedChild` is reported once per *name*, not once per edge — the name is what the
//! schema rejects — and it short-circuits, so a rejected name never also reports as a
//! duplicate.
//!
//! Both [`crate::entity::EntityLoader`] and [`crate::live_entity::LiveEntity`] resolve
//! through here. That shared path is what makes them agree, which is defect C2 in
//! `docs/storage-layout-v2.md`.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::bail;
use inscenerator_xfs::Xfs;

use crate::entity::EntityPath;
use crate::findings::{Finding, FindingKind, FindingSink};
use crate::placement::{self, Edge, RESERVED_DOT_SUFFIXES, RESERVED_SLASH_NAMES};
use crate::schema::{ChildMatch, CompiledType};

/// A child, after both passes have been reconciled against the schema.
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedChild {
    /// The child's name. Not unique among siblings on its own — see `edge`.
    pub name: String,
    /// The logical path. Unique among siblings, since `(edge, name)` is the key.
    pub path: EntityPath,
    /// The edge this child sits on, which may not be the one its rule declares.
    pub edge: Edge,
    /// The type its rule assigns. `None` for an `allow_additional` child, which matched
    /// no rule and so has no declared type (§7.2).
    pub node_type: Option<String>,
    /// Index of the rule that matched, naming the rule in per-rule findings (§4.2).
    pub rule_index: Option<usize>,
}

/// Names of the form `stem.<name>`, found in the parent directory.
///
/// The root has no filename to prefix, so it has no dot children; callers must not run
/// this pass for it.
pub(crate) fn dot_child_names(
    fs: &dyn Xfs,
    base_path: &Path,
    entity_path: &EntityPath,
) -> anyhow::Result<BTreeSet<String>> {
    let p = placement::stem(base_path, entity_path);
    let Some(p_str) = p.to_str() else {
        bail!("path {p:?} not convertable to string.")
    };
    let Some(entity_dir) = p.parent() else {
        bail!("Entity path {:?} has no parent", p);
    };

    let mut names = BTreeSet::new();
    let prefix = format!("{}.", p_str);
    for de in fs.read_dir(entity_dir)? {
        let entry_path = de?.path();
        let Some(entry_path_str) = entry_path.to_str() else {
            bail!("child path {entry_path:?} not convertable to string")
        };
        let Some(suffix) = entry_path_str.strip_prefix(&prefix) else {
            continue;
        };
        // The entity's own files, not children of it.
        if RESERVED_DOT_SUFFIXES.contains(&suffix) {
            continue;
        }
        // `name.rest` is the child `name`; whatever follows belongs to it, not to us.
        let name = match suffix.split_once('.') {
            Some((name, _)) => name,
            None => suffix,
        };
        names.insert(name.to_string());
    }
    Ok(names)
}

/// Names found inside the stem directory. Empty when there is no such directory.
pub(crate) fn slash_child_names(
    fs: &dyn Xfs,
    base_path: &Path,
    entity_path: &EntityPath,
) -> anyhow::Result<BTreeSet<String>> {
    let entity_dir = placement::stem(base_path, entity_path);
    if !fs.is_dir(&entity_dir) {
        return Ok(BTreeSet::new());
    }

    let mut names = BTreeSet::new();
    for de in fs.read_dir(&entity_dir)? {
        let entry_path = de?.path();
        if let Some(filename) = entry_path.file_name().and_then(|s| s.to_str()) {
            // The entity's own files, not children of it.
            if RESERVED_SLASH_NAMES.contains(&filename) {
                continue;
            }
        }
        let Some(name) = entry_path.file_prefix().and_then(|s| s.to_str()) else {
            bail!("Entry path {:?} has no filename", entry_path);
        };
        names.insert(name.to_string());
    }
    Ok(names)
}

/// Every child of `path`, reconciled against `ctype`, with drift reported to `sink`.
///
/// Ordered by name, and within a name, dot before slash.
///
/// # Errors
///
/// Returns an error on I/O failure, or when a finding's severity under the sink's policy
/// is [`crate::findings::Severity::Error`].
pub fn resolve_children(
    fs: &dyn Xfs,
    base_path: &Path,
    path: &EntityPath,
    ctype: &CompiledType,
    sink: &mut FindingSink,
) -> anyhow::Result<Vec<ResolvedChild>> {
    let is_root = path.entries.is_empty();

    // (seen by the dot pass, seen by the slash pass). Merging here is what stops one
    // name being resolved twice, once per pass.
    let mut seen: BTreeMap<String, (bool, bool)> = BTreeMap::new();
    if !is_root {
        for name in dot_child_names(fs, base_path, path)? {
            seen.entry(name).or_default().0 = true;
        }
    }
    for name in slash_child_names(fs, base_path, path)? {
        seen.entry(name).or_default().1 = true;
    }

    let mut resolved = Vec::new();
    for (name, (on_dot, on_slash)) in seen {
        let (node_type, rule_index, intended_edge) = match ctype.match_child(&name) {
            ChildMatch::Ignored => continue,
            ChildMatch::Unexpected => {
                // The schema rejects the name itself, so the edge it was found on is
                // beside the point. Reported once, and no further questions are asked.
                sink.report(Finding {
                    path: path.clone(),
                    kind: FindingKind::UnexpectedChild { name },
                })?;
                continue;
            }
            // No rule matched, so nothing declares this child's type or edge (§7.2).
            ChildMatch::Additional => (None, None, None),
            ChildMatch::Matched(r) => (
                Some(r.rule.node_type.clone()),
                Some(r.index),
                Some(r.rule.edge),
            ),
        };

        if on_dot && on_slash {
            // Two entities at two addresses that happen to share a name (§4.4). Both are
            // real; only name-based lookup is ambiguous, and that fails at the lookup.
            sink.report(Finding {
                path: path.clone(),
                kind: FindingKind::DuplicateChildName {
                    name: name.clone(),
                    dot_path: placement::stem(base_path, &path.extend_dot(&name)),
                    slash_path: placement::stem(base_path, &path.extend_slash(&name)),
                },
            })?;
        }

        for edge in [Edge::Dot, Edge::Slash] {
            let present = match edge {
                Edge::Dot => on_dot,
                Edge::Slash => on_slash,
            };
            if !present {
                continue;
            }
            if let (Some(intended), Some(rule_index)) = (intended_edge, rule_index) {
                if intended != edge {
                    sink.report(Finding {
                        path: path.clone(),
                        kind: FindingKind::EdgeNonconformance {
                            name: name.clone(),
                            actual: edge,
                            intended,
                            rule_index,
                        },
                    })?;
                }
            }
            resolved.push(ResolvedChild {
                name: name.clone(),
                path: placement::child_path(path, &name, edge),
                edge,
                node_type: node_type.clone(),
                rule_index,
            });
        }
    }

    report_per_rule(ctype, &resolved, path, sink)?;
    Ok(resolved)
}

/// §4.2 and D1: what each rule has to say about the children that matched it.
fn report_per_rule(
    ctype: &CompiledType,
    resolved: &[ResolvedChild],
    path: &EntityPath,
    sink: &mut FindingSink,
) -> anyhow::Result<()> {
    for rule in &ctype.rules {
        let matched: Vec<&ResolvedChild> = resolved
            .iter()
            .filter(|c| c.rule_index == Some(rule.index))
            .collect();

        // §4.2: a split is a property of one rule, not of the node. Two rules with
        // different edges are exactly what the schema asked for.
        let names_on = |edge: Edge| -> Vec<String> {
            matched
                .iter()
                .filter(|c| c.edge == edge)
                .map(|c| c.name.clone())
                .collect()
        };
        let dot = names_on(Edge::Dot);
        let slash = names_on(Edge::Slash);
        if !dot.is_empty() && !slash.is_empty() {
            sink.report(Finding {
                path: path.clone(),
                kind: FindingKind::SplitChildEdge {
                    rule_index: rule.index,
                    slash,
                    dot,
                },
            })?;
        }

        // D1: reported, never enforced — every child resolved regardless.
        if matched.is_empty() && rule.rule.required {
            sink.report(Finding {
                path: path.clone(),
                kind: FindingKind::MissingRequiredChild {
                    rule_index: rule.index,
                    name_regex: rule.rule.name_regex.clone(),
                },
            })?;
        }
        if matched.len() > 1 && !rule.rule.multiple {
            sink.report(Finding {
                path: path.clone(),
                kind: FindingKind::MultipleChildrenNotAllowed {
                    rule_index: rule.index,
                    names: matched.iter().map(|c| c.name.clone()).collect(),
                },
            })?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityPath;
    use crate::findings::{FindingKind, FindingKindId, FindingPolicy, FindingSink, Severity};
    use crate::placement::Edge;
    use crate::schema::{ChildEntityRules, EntityTypeDescription, Schema};
    use inscenerator_xfs::mockfs;
    use std::path::{Path, PathBuf};

    /// Every file named here exists; every directory implied by one exists.
    fn fs_with(files: &[&str]) -> mockfs::MockFS {
        let mut fs = mockfs::MockFS::new();
        for f in files {
            let p = PathBuf::from(f);
            fs.create_dir_all(p.parent().unwrap()).unwrap();
            fs.add_r(&p, b"x".to_vec()).unwrap();
        }
        fs
    }

    fn rule(name_regex: &str, edge: Edge) -> ChildEntityRules {
        ChildEntityRules {
            name_regex: name_regex.into(),
            node_type: "Child".into(),
            required: false,
            multiple: true,
            edge,
        }
    }

    /// A schema whose type `T` has the given rules; `ignore` and `allow_additional`
    /// default to the strict end so a test opting into them says so.
    fn schema_of(rules: Vec<ChildEntityRules>) -> Schema {
        schema_full(rules, false, vec![])
    }

    fn schema_full(
        rules: Vec<ChildEntityRules>,
        allow_additional: bool,
        ignore: Vec<String>,
    ) -> Schema {
        let mut s = Schema::new();
        s.add_entity_type(EntityTypeDescription {
            name: "T".into(),
            children: rules,
            allow_additional,
            ignore,
            layout: None,
        })
        .unwrap();
        s
    }

    /// Resolves the children of `base/ch1`, typed `T`.
    fn resolve(
        fs: &mockfs::MockFS,
        schema: &Schema,
        sink: &mut FindingSink,
    ) -> anyhow::Result<Vec<ResolvedChild>> {
        resolve_children(
            fs,
            Path::new("base"),
            &EntityPath::empty().extend_slash("ch1"),
            schema.compiled("T").unwrap(),
            sink,
        )
    }

    fn sink() -> FindingSink {
        FindingSink::new(FindingPolicy::default())
    }

    fn names(kids: &[ResolvedChild]) -> Vec<(&str, Edge)> {
        kids.iter().map(|c| (c.name.as_str(), c.edge)).collect()
    }

    fn has<F: Fn(&FindingKind) -> bool>(sink: &FindingSink, f: F) -> bool {
        sink.findings().iter().any(|x| f(&x.kind))
    }

    /// C2: a name on both edges is *two* children — distinct addresses that merely
    /// share a name — and the collision is reported rather than resolved away.
    #[test]
    fn a_name_on_both_edges_yields_two_children_and_one_finding() {
        let fs = fs_with(&["base/ch1.notes.md", "base/ch1/notes.md"]);
        let schema = schema_of(vec![rule("^notes$", Edge::Slash)]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(names(&kids), vec![("notes", Edge::Dot), ("notes", Edge::Slash)]);
        assert_eq!(
            kids.iter().map(|c| c.path.clone()).collect::<Vec<_>>(),
            vec![
                EntityPath::empty().extend_slash("ch1").extend_dot("notes"),
                EntityPath::empty().extend_slash("ch1").extend_slash("notes"),
            ],
            "(edge, name) is the key: the two share a name but not an address"
        );
        assert!(has(&sink, |k| matches!(
            k,
            FindingKind::DuplicateChildName { name, dot_path, slash_path }
                if name == "notes"
                    && dot_path == &PathBuf::from("base/ch1.notes")
                    && slash_path == &PathBuf::from("base/ch1/notes")
        )));
    }

    /// §3: a child on an edge its rule did not declare still loads, and is reported.
    /// The check is per child, so the dot twin above is reported here on its own.
    #[test]
    fn a_child_on_the_undeclared_edge_still_loads_and_is_reported() {
        let fs = fs_with(&["base/ch1.notes.md"]);
        let schema = schema_of(vec![rule("^notes$", Edge::Slash)]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(names(&kids), vec![("notes", Edge::Dot)]);
        assert!(has(&sink, |k| matches!(
            k,
            FindingKind::EdgeNonconformance {
                name, actual: Edge::Dot, intended: Edge::Slash, rule_index: 0,
            } if name == "notes"
        )));
    }

    /// §4.2: a split is observed per *rule*, not per node. Rule 0's children sit on
    /// both edges and are reported; rule 1's sit on the other edge and are not.
    #[test]
    fn a_split_is_reported_per_rule_not_per_node() {
        let fs = fs_with(&[
            "base/ch1/010-intro.md",
            "base/ch1.020-body.md",
            "base/ch1.notes.md",
        ]);
        let schema = schema_of(vec![
            rule(r"^\d{3}-", Edge::Slash),
            rule("^notes$", Edge::Dot),
        ]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();
        assert_eq!(kids.len(), 3);

        let splits: Vec<_> = sink
            .findings()
            .iter()
            .filter_map(|f| match &f.kind {
                FindingKind::SplitChildEdge { rule_index, slash, dot } => {
                    Some((*rule_index, slash.clone(), dot.clone()))
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            splits,
            vec![(0, vec!["010-intro".to_string()], vec!["020-body".to_string()])]
        );
    }

    /// §7.4 and §7.2: a name is dropped silently when ignored, dropped loudly when it
    /// matches nothing under `allow_additional = false`, and kept with no declared type
    /// when `allow_additional` lets it through.
    #[test]
    fn a_name_matching_no_rule_is_ignored_rejected_or_untyped() {
        // (label, allow_additional, ignore, child kept, UnexpectedChild reported)
        let cases: &[(&str, bool, &[&str], bool, bool)] = &[
            ("ignored", false, &[r"^\."], false, false),
            ("rejected", false, &[], false, true),
            ("additional", true, &[], true, false),
        ];

        for (label, allow_additional, ignore, keeps_child, reports) in cases {
            let fs = fs_with(&["base/ch1/.hidden.md"]);
            let ignore = ignore.iter().map(|s| s.to_string()).collect();
            let schema = schema_full(vec![], *allow_additional, ignore);
            // UnexpectedChild is an Error by default, which would abort the resolve.
            // Downgraded here so all three rows share one shape and the question stays
            // "which names produce a child", not "which policy fails".
            let mut sink = FindingSink::new(
                FindingPolicy::default().with(FindingKindId::UnexpectedChild, Severity::Warn),
            );

            let kids = resolve(&fs, &schema, &mut sink).unwrap();

            assert_eq!(kids.len(), usize::from(*keeps_child), "{}", label);
            if *keeps_child {
                // §7.2: no rule matched, so nothing declares this child's type or edge.
                assert_eq!(kids[0].node_type, None, "{}", label);
                assert_eq!(kids[0].rule_index, None, "{}", label);
            }
            assert_eq!(
                has(&sink, |k| matches!(k, FindingKind::UnexpectedChild { .. })),
                *reports,
                "{}",
                label
            );
        }
    }

    /// D1 / C9: `required` and `multiple` are reported, never enforced — every child
    /// still resolves.
    #[test]
    fn required_and_multiple_are_reported_not_enforced() {
        let fs = fs_with(&["base/ch1/a.md", "base/ch1/b.md"]);
        let mut only_one = rule("^[ab]$", Edge::Slash);
        only_one.multiple = false;
        let mut needed = rule("^zzz$", Edge::Slash);
        needed.required = true;
        let schema = schema_of(vec![only_one, needed]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(names(&kids), vec![("a", Edge::Slash), ("b", Edge::Slash)]);
        assert!(has(&sink, |k| matches!(
            k,
            FindingKind::MultipleChildrenNotAllowed { rule_index: 0, names }
                if names == &vec!["a".to_string(), "b".to_string()]
        )));
        assert!(has(&sink, |k| matches!(
            k,
            FindingKind::MissingRequiredChild { rule_index: 1, .. }
        )));
    }

    /// C8: a name matching a rule with no entity behind it resolves without complaint.
    /// Whether it becomes an entity is the loader's call, not discovery's.
    #[test]
    fn a_matched_name_with_no_entity_behind_it_is_not_an_error() {
        let fs = fs_with(&["base/ch1/.gitkeep"]);
        let schema = schema_of(vec![rule(r"^\.gitkeep$", Edge::Slash)]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(names(&kids), vec![(".gitkeep", Edge::Slash)]);
        assert!(sink.findings().is_empty());
    }

    /// §3: the root has no filename to prefix, so it has no dot children — a file that
    /// looks like one belongs to whatever sits beside the root, not to the root.
    #[test]
    fn the_root_has_no_dot_children() {
        let fs = fs_with(&["base.notes.md", "base/a.md"]);
        let schema = schema_of(vec![rule("^[an].*$", Edge::Slash)]);
        let mut sink = sink();

        let kids = resolve_children(
            &fs,
            Path::new("base"),
            &EntityPath::empty(),
            schema.compiled("T").unwrap(),
            &mut sink,
        )
        .unwrap();

        assert_eq!(names(&kids), vec![("a", Edge::Slash)]);
    }

    /// §7.1: a matched child carries the rule's type and the index that identifies the
    /// rule in every per-rule finding.
    #[test]
    fn a_matched_child_carries_its_rules_type_and_index() {
        let fs = fs_with(&["base/ch1/notes.md"]);
        let schema = schema_of(vec![rule("^never$", Edge::Slash), rule("^notes$", Edge::Slash)]);
        let mut sink = sink();

        let kids = resolve(&fs, &schema, &mut sink).unwrap();

        assert_eq!(kids[0].node_type.as_deref(), Some("Child"));
        assert_eq!(kids[0].rule_index, Some(1));
    }
}
