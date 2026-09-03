//! Drift reporting. §9.1 of `docs/storage-layout-v2.md`.
//!
//! Two different things are reported here. **Nonconformance** is "this node disagrees
//! with its schema" — it has an obvious fix, and a tree humans edit by hand is full of
//! it, so it defaults to [`Severity::Warn`]. **Ambiguity** is "this node does not agree
//! with itself" — there is no resolution the library can pick without inventing one, so
//! it defaults to [`Severity::Error`].
//!
//! Both travel the same channel. A [`FindingSink`] applies a [`FindingPolicy`] as each
//! finding arrives: `Ignore` drops it, `Warn` records it, `Error` records it *and* fails
//! the call that produced it. [`FindingSink::observe`] bypasses the policy entirely, for
//! callers whose whole job is to report rather than to enforce.

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

use crate::entity::EntityPath;
use crate::placement::{ContentLocation, Edge, MetaLocation};

/// What a policy says should happen when a finding is reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Not recorded at all.
    Ignore,
    /// Recorded on the entity, or returned by `issues()`.
    Warn,
    /// Recorded, and the call that produced it returns `Err`.
    Error,
}

/// A [`FindingKind`] with its payload stripped, so it can be used as a policy key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FindingKindId {
    MalformedMetadata,
    MetadataKeyConflict,
    SplitMetadata,
    ContentLocationNonconformance,
    MetadataLocationNonconformance,
    EdgeNonconformance,
    SplitChildEdge,
    StrayContent,
    DuplicateChildName,
    MissingRequiredChild,
    MultipleChildrenNotAllowed,
    UnexpectedChild,
    TypeMismatch,
}

impl FindingKindId {
    /// Every kind. A policy that speaks for "everything" enumerates this.
    pub const ALL: [FindingKindId; 13] = [
        FindingKindId::MalformedMetadata,
        FindingKindId::MetadataKeyConflict,
        FindingKindId::SplitMetadata,
        FindingKindId::ContentLocationNonconformance,
        FindingKindId::MetadataLocationNonconformance,
        FindingKindId::EdgeNonconformance,
        FindingKindId::SplitChildEdge,
        FindingKindId::StrayContent,
        FindingKindId::DuplicateChildName,
        FindingKindId::MissingRequiredChild,
        FindingKindId::MultipleChildrenNotAllowed,
        FindingKindId::UnexpectedChild,
        FindingKindId::TypeMismatch,
    ];
}

/// What was observed, and enough detail to act on it.
#[derive(Debug, Clone, PartialEq)]
pub enum FindingKind {
    /// A metadata source exists but does not parse. D3.
    MalformedMetadata {
        location: MetaLocation,
        path: Option<PathBuf>,
        error: String,
    },
    /// The same key in two sources with different values — §4.5's unroutable case.
    MetadataKeyConflict {
        key: String,
        locations: Vec<MetaLocation>,
    },
    /// Metadata is spread across several sources. Ambiguity, not nonconformance:
    /// every key still routes, but the node's shape was never intendable.
    SplitMetadata {
        keys_by_location: Vec<(MetaLocation, Vec<String>)>,
    },
    /// Content is not where the intended layout says it should be.
    ContentLocationNonconformance {
        actual: ContentLocation,
        intended: ContentLocation,
    },
    /// A sidecar is not where the intended layout says it should be.
    MetadataLocationNonconformance {
        actual: MetaLocation,
        intended: MetaLocation,
    },
    /// A child sits on an edge its rule did not declare. §3.
    EdgeNonconformance {
        name: String,
        actual: Edge,
        intended: Edge,
        rule_index: usize,
    },
    /// One rule's children are split across both edges. §4.2.
    SplitChildEdge {
        rule_index: usize,
        slash: Vec<String>,
        dot: Vec<String>,
    },
    /// The losing half of an `S.md` + `S/content.md` pair. §4.4.
    StrayContent {
        path: PathBuf,
        location: ContentLocation,
    },
    /// Two children share a name across the edges. §4.4.
    ///
    /// Both load — they are distinct addresses that merely share a name, and `(edge, name)`
    /// is what identifies a child. Only name-based lookup is ambiguous, and that fails at
    /// the call rather than here.
    DuplicateChildName {
        name: String,
        dot_path: PathBuf,
        slash_path: PathBuf,
    },
    /// A `required` rule with no matching child. D1.
    MissingRequiredChild {
        rule_index: usize,
        name_regex: String,
    },
    /// A non-`multiple` rule with several matching children. D1.
    MultipleChildrenNotAllowed {
        rule_index: usize,
        names: Vec<String>,
    },
    /// A child name matching no rule, under `allow_additional = false`.
    UnexpectedChild { name: String },
    /// Metadata's `type` key disagrees with the type the parent's rule assigned. §7.1.
    TypeMismatch { expected: String, found: String },
}

impl FindingKind {
    pub fn id(&self) -> FindingKindId {
        match self {
            FindingKind::MalformedMetadata { .. } => FindingKindId::MalformedMetadata,
            FindingKind::MetadataKeyConflict { .. } => FindingKindId::MetadataKeyConflict,
            FindingKind::SplitMetadata { .. } => FindingKindId::SplitMetadata,
            FindingKind::ContentLocationNonconformance { .. } => {
                FindingKindId::ContentLocationNonconformance
            }
            FindingKind::MetadataLocationNonconformance { .. } => {
                FindingKindId::MetadataLocationNonconformance
            }
            FindingKind::EdgeNonconformance { .. } => FindingKindId::EdgeNonconformance,
            FindingKind::SplitChildEdge { .. } => FindingKindId::SplitChildEdge,
            FindingKind::StrayContent { .. } => FindingKindId::StrayContent,
            FindingKind::DuplicateChildName { .. } => FindingKindId::DuplicateChildName,
            FindingKind::MissingRequiredChild { .. } => FindingKindId::MissingRequiredChild,
            FindingKind::MultipleChildrenNotAllowed { .. } => {
                FindingKindId::MultipleChildrenNotAllowed
            }
            FindingKind::UnexpectedChild { .. } => FindingKindId::UnexpectedChild,
            FindingKind::TypeMismatch { .. } => FindingKindId::TypeMismatch,
        }
    }
}

impl fmt::Display for FindingKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FindingKind::MalformedMetadata { location, path, error } => match path {
                Some(p) => write!(f, "Malformed metadata at {:?} ({:?}): {}", location, p, error),
                None => write!(f, "Malformed metadata at {:?}: {}", location, error),
            },
            FindingKind::MetadataKeyConflict { key, locations } => write!(
                f,
                "Metadata key '{}' has differing values in {:?}",
                key, locations
            ),
            FindingKind::SplitMetadata { keys_by_location } => write!(
                f,
                "Metadata is split across {} sources: {:?}",
                keys_by_location.len(),
                keys_by_location
            ),
            FindingKind::ContentLocationNonconformance { actual, intended } => write!(
                f,
                "Content is {:?} but its layout intends {:?}",
                actual, intended
            ),
            FindingKind::MetadataLocationNonconformance { actual, intended } => write!(
                f,
                "Metadata is {:?} but its layout intends {:?}",
                actual, intended
            ),
            FindingKind::EdgeNonconformance { name, actual, intended, rule_index } => write!(
                f,
                "Child '{}' is on the {:?} edge but rule {} declares {:?}",
                name, actual, rule_index, intended
            ),
            FindingKind::SplitChildEdge { rule_index, slash, dot } => write!(
                f,
                "Rule {} has children on both edges: slash {:?}, dot {:?}",
                rule_index, slash, dot
            ),
            FindingKind::StrayContent { path, location } => {
                write!(f, "Stray {:?} content file at {:?}", location, path)
            }
            FindingKind::DuplicateChildName { name, dot_path, slash_path } => write!(
                f,
                "Two children are named '{}' ({:?} and {:?}); look them up by edge",
                name, dot_path, slash_path
            ),
            FindingKind::MissingRequiredChild { rule_index, name_regex } => write!(
                f,
                "No child matches required rule {} ({})",
                rule_index, name_regex
            ),
            FindingKind::MultipleChildrenNotAllowed { rule_index, names } => write!(
                f,
                "Rule {} is not 'multiple' but matched {:?}",
                rule_index, names
            ),
            FindingKind::UnexpectedChild { name } => {
                write!(f, "Unexpected child '{}' matches no rule", name)
            }
            FindingKind::TypeMismatch { expected, found } => {
                write!(f, "Expected type '{}' but metadata declares '{}'", expected, found)
            }
        }
    }
}

/// A finding, with the entity it was observed on.
#[derive(Debug, Clone, PartialEq)]
pub struct Finding {
    pub path: EntityPath,
    pub kind: FindingKind,
}

impl fmt::Display for Finding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {:?}", self.kind, self.path.local_path())
    }
}

/// Per-kind severity. Tolerant by default; see the module docs for the split.
#[derive(Debug, Clone, Default)]
pub struct FindingPolicy {
    overrides: HashMap<FindingKindId, Severity>,
}

impl FindingPolicy {
    pub fn severity(&self, kind: &FindingKind) -> Severity {
        self.severity_of(kind.id())
    }

    pub fn severity_of(&self, id: FindingKindId) -> Severity {
        if let Some(s) = self.overrides.get(&id) {
            return *s;
        }
        match id {
            // No resolution the library can pick, or a flat schema violation.
            FindingKindId::MetadataKeyConflict
            | FindingKindId::UnexpectedChild
            | FindingKindId::TypeMismatch => Severity::Error,
            // Everything else is drift: normal in a tree humans edit by hand.
            _ => Severity::Warn,
        }
    }

    /// Overrides one kind, replacing any previous ruling for it.
    #[must_use]
    pub fn with(mut self, id: FindingKindId, severity: Severity) -> Self {
        self.overrides.insert(id, severity);
        self
    }

    fn uniform(severity: Severity) -> Self {
        FindingPolicy {
            overrides: FindingKindId::ALL.iter().map(|id| (*id, severity)).collect(),
        }
    }

    /// Every kind is an error. For CI and scripted batch runs.
    pub fn strict() -> Self {
        Self::uniform(Severity::Error)
    }

    /// Nothing is recorded. For consumers that only want the data.
    pub fn silent() -> Self {
        Self::uniform(Severity::Ignore)
    }
}

/// Accumulates findings for one node, applying the policy as each is reported.
#[derive(Debug)]
pub struct FindingSink {
    policy: FindingPolicy,
    findings: Vec<Finding>,
}

impl FindingSink {
    pub fn new(policy: FindingPolicy) -> Self {
        FindingSink {
            policy,
            findings: Vec::new(),
        }
    }

    /// Records the finding per policy.
    ///
    /// # Errors
    ///
    /// Returns an error when the finding's severity is [`Severity::Error`]. The finding
    /// is recorded first, so a caller inspecting the sink afterwards sees what stopped it.
    pub fn report(&mut self, finding: Finding) -> anyhow::Result<()> {
        match self.policy.severity(&finding.kind) {
            Severity::Ignore => Ok(()),
            Severity::Warn => {
                self.findings.push(finding);
                Ok(())
            }
            Severity::Error => {
                let msg = finding.to_string();
                self.findings.push(finding);
                Err(anyhow::anyhow!(msg))
            }
        }
    }

    /// Records regardless of severity, and never fails. Used by `issues()`, whose whole
    /// job is to report rather than to enforce.
    pub fn observe(&mut self, finding: Finding) {
        self.findings.push(finding);
    }

    pub fn findings(&self) -> &[Finding] {
        &self.findings
    }

    pub fn into_findings(self) -> Vec<Finding> {
        self.findings
    }

    pub fn policy(&self) -> &FindingPolicy {
        &self.policy
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityPath;
    use crate::placement::{ContentLocation, MetaLocation};

    fn stray_content() -> Finding {
        Finding {
            path: EntityPath::empty().extend_slash("ch1"),
            kind: FindingKind::StrayContent {
                path: "base/ch1/content.md".into(),
                location: ContentLocation::Inside,
            },
        }
    }

    /// D6: drift is tolerated by default; what has no defined resolution is not.
    ///
    /// The table is exhaustive over `FindingKindId`, so a new kind cannot be added
    /// without a deliberate ruling on its default severity.
    #[test]
    fn the_default_policy_warns_on_drift_and_errors_on_ambiguity() {
        use FindingKindId::*;
        use Severity::{Error, Warn};

        let expected: &[(FindingKindId, Severity)] = &[
            // Drift: normal in a tree humans edit by hand, and each has an obvious fix.
            (MalformedMetadata, Warn),
            (SplitMetadata, Warn),
            (ContentLocationNonconformance, Warn),
            (MetadataLocationNonconformance, Warn),
            (EdgeNonconformance, Warn),
            (SplitChildEdge, Warn),
            (StrayContent, Warn),
            (DuplicateChildName, Warn),
            (MissingRequiredChild, Warn),
            (MultipleChildrenNotAllowed, Warn),
            // No defined resolution without a human, or a flat schema violation.
            (MetadataKeyConflict, Error),
            (UnexpectedChild, Error),
            (TypeMismatch, Error),
        ];

        let policy = FindingPolicy::default();
        for (id, want) in expected {
            assert_eq!(policy.severity_of(*id), *want, "{:?}", id);
        }
        assert_eq!(
            expected.len(),
            FindingKindId::ALL.len(),
            "a new FindingKind needs a default severity ruling in this table"
        );
    }

    /// D6: severity is per kind, and an override replaces whatever came before it.
    #[test]
    fn severity_is_configurable_per_kind() {
        let policy = FindingPolicy::default().with(FindingKindId::StrayContent, Severity::Error);
        assert_eq!(policy.severity(&stray_content().kind), Severity::Error);
        // A neighbouring kind is untouched.
        assert_eq!(
            policy.severity_of(FindingKindId::DuplicateChildName),
            Severity::Warn
        );

        let policy = policy.with(FindingKindId::StrayContent, Severity::Ignore);
        assert_eq!(policy.severity(&stray_content().kind), Severity::Ignore);
    }

    /// The two presets are all-or-nothing across every kind.
    #[test]
    fn strict_errors_on_everything_and_silent_ignores_everything() {
        for id in FindingKindId::ALL {
            assert_eq!(FindingPolicy::strict().severity_of(id), Severity::Error, "{:?}", id);
            assert_eq!(FindingPolicy::silent().severity_of(id), Severity::Ignore, "{:?}", id);
        }
    }

    /// `report` applies the policy: `Ignore` drops, `Warn` records, `Error` records
    /// *and then* fails, so a caller inspecting after the failure sees what stopped it.
    #[test]
    fn report_drops_ignored_records_warnings_and_fails_on_errors() {
        let mut sink = FindingSink::new(FindingPolicy::silent());
        assert!(sink.report(stray_content()).is_ok());
        assert!(sink.findings().is_empty());

        let mut sink = FindingSink::new(FindingPolicy::default());
        assert!(sink.report(stray_content()).is_ok());
        assert_eq!(sink.findings(), &[stray_content()]);

        let mut sink = FindingSink::new(FindingPolicy::strict());
        let err = sink.report(stray_content()).unwrap_err().to_string();
        assert!(err.contains("content.md"), "must name the finding: {}", err);
        assert!(err.contains("ch1"), "must name the entity: {}", err);
        assert_eq!(sink.findings(), &[stray_content()]);
    }

    /// `issues()` exists to report rather than to enforce, so it needs a channel that
    /// records regardless of policy and never short-circuits.
    #[test]
    fn observe_records_regardless_of_policy() {
        let mut sink = FindingSink::new(FindingPolicy::silent());
        sink.observe(stray_content());
        sink.observe(Finding {
            path: EntityPath::empty(),
            kind: FindingKind::MetadataKeyConflict {
                key: "type".into(),
                locations: vec![MetaLocation::InHeader, MetaLocation::ParallelSidecar],
            },
        });
        assert_eq!(sink.into_findings().len(), 2);
    }
}
