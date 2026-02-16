# Changelog

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
