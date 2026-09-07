# Changelog

## v0.2.0

A new storage layout. The schema now declares **where** things go, so a caller can create a child
without knowing the on-disk convention. See `docs/storage-layout.md` for the layout as it now is,
and `docs/migration-0.1-to-0.2.md` for what changed.

### Breaking
- `LiveEntity::create_child` and `ChildBuilder::with_child` take a **name** (`&str`), not an
  `EntityPathEntry`. The edge is resolved from the schema, or overridden with `with_edge`.
- `ChildContentLayout` is deleted, along with `with_content_inside` / `with_content_parallel` /
  `with_metadata_inside` / `with_metadata_parallel`. Layout comes from the child's type, or from
  `with_layout`.
- `EntityMeta` is a list of observed sources rather than an enum of one. `metadata()` becomes
  `merged()`; construct with `EntityMeta::of`, or `::at` / `::parallel` / `::inside` /
  `::in_header` for the single-source cases.
- `LiveEntity::set_metadata(EntityMeta)` is replaced by `set_meta_key`, `remove_meta_key`,
  `set_metadata_at` and `clear_metadata`. A source list is an observation, not an instruction.
- `Schema::add_entity_type` returns `anyhow::Result<()>`: it compiles the type's regexes, so an
  invalid pattern fails there rather than at a later lookup.
- `EntityTypeDescription::child_type` is deleted; `CompiledType::match_child` is the one rule
  matcher.
- `EntityTypeDescription.ignore` entries are **regexes**, matched like `children` rules, where
  they were previously exact strings.
- `EntityTypeDescription` gains `layout`, `ChildEntityRules` gains `edge`, `Entity` gains
  `layout` and `findings`.
- `EntityLoader::try_load_entity` takes an `inherited_layout`; new callers should use
  `try_load_root`.
- `LiveEntity` gains an `inherited_layout` field, set when a handle is reached through its
  parent. `LiveEntity::new` builds a root handle, so it starts at `Layout::Inside`.

### Fixed
- A dot-child's parallel sidecar was computed by *substituting* the extension, so
  `a/b.review` resolved onto its parent's `a/b.meta.toml`. Writing a dot-child overwrote its
  parent's metadata, and the loader could not read back what `LiveEntity` wrote. Every suffix is
  now appended; `with_extension` appears nowhere in `src/`.
- `children()` and `child()` disagreed about a name present on both edges. Both now
  resolve through `discovery::resolve_children`.
- Layout was inferred from the edge, which made a spine file with a folder of children
  beside it unwritable through the library.
- The schema had no way to say which edge a child attaches on.
- There were two rule-matching implementations, which could disagree.
- `ignore` was exact-match while `children` was regex.
- A node with mixed layouts failed to load; it now loads, is reported, and stays mixed
  when an unrelated key is written.
- A rule that matched a name with no files behind it was an error.
- `required` / `multiple` were unenforced *and* unreported; they are now reported.
- A metadata file that did not parse aborted the load. It is now retained with its raw
  text and error, and written back verbatim.
- The README described root metadata as `project.meta.toml`. The root is always
  `inside`, so its metadata is `meta.toml` inside the root directory.

### Added
- `placement` — the single authority for turning an `EntityPath` into a path on disk.
- `findings` — `Finding`, `FindingKind`, `Severity` and a per-kind `FindingPolicy`
  (`strict()` / `silent()` presets). Reading is tolerant: drift is reported, not fatal.
- `discovery` — one child resolver, with cross-pass dedup, intent tie-breaking and per-rule edge
  observation, shared by the eager loader and the lazy handle.
- `reading::read_node` — one implementation of a node's type, layout, content and metadata.
- `Entity::all_findings`, `LiveEntity::issues`, `LiveEntity::intended_layout`,
  `LiveEntity::observed`.
- `ChildBuilder::with_edge`, `with_layout`, `with_metadata_at`.
- `LiveEntity::with_policy` and `EntityLoader::with_policy` set the drift policy;
  `LiveEntity::load_from_root_with` takes a schema directly instead of reading `schema.toml`.

## v0.1.7
- Exposed `parse_front_matter` as a public API.
- Added `LiveEntity::child()` for direct child access by name (checks both slash-path and dot-path forms).
- Added ignore list to `EntityTypeDescription` — schema can now list directory/file names to skip during child discovery (e.g. `ignore = ["booker-data"]`).

## v0.1.6
- Improved `LiveEntity` child creation options.
- Replaced `Rc<RefCell<dyn Xfs>>` with `Arc<Mutex<dyn Xfs + Send + Sync>>` for thread-safe filesystem access.

## v0.1.5
- Added `Entity::metadata_mut()`
- Allow YAML front-matter as metadata source. 

## v0.1.4
- Entity schemas cab ne loaded from from `schema.toml` files.
- Support for new "Live" style entities, that allow immediate read-write.


## v0.1.3

### Added
- Support for metadata embedded in entity content headers using fenced TOML blocks.
- Support `Auto` entity types, where type is deduced from the metadata.
- Devcontainer configuration for standardized development environments.

### Changed
- Refactored internal child discovery logic for better path handling.
- Updated `inscenerator-xfs` dependency.

## v0.1.2

???


## v0.1.1

???

## v0.1.0

- Initial release.
- Core entity loading and writing functionality.
- Support for both "Inside" (directory-based) and "Parallel" (file-based) entity storage.
- Filesystem abstraction via the `Xfs` trait.
