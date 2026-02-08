# Changelog

All notable changes to this project will be documented in this file.

## v0.1.3

### Added
- Support for metadata embedded in entity content headers using fenced TOML blocks.
- New `InHeader` variant to `EntityMeta` to support content-embedded metadata.
- Support for the `Auto` entity type, allowing `EntityLoader` to resolve types dynamically from metadata.
- Type validation to ensure entity metadata matches the expected type during loading.
- Schema-driven child entity discovery and type assignment using `EntityTypeDescription`.
- Comprehensive "Entity Mapping" documentation in README.md.
- Devcontainer configuration for standardized development environments.
- Support for loading entity schemas from `schema.toml` files.

### Changed
- Updated `EntityLoader` and `EntityWriter` to support the new metadata and type resolution features.
- Refactored internal child discovery logic for better path handling.
- Enhanced internal testing utilities and coverage.
- Updated `inscenerator-xfs` dependency.

## v0.1.2

- Version missing from repository.

## v0.1.1

- Version missing from repository.

## v0.1.0

- Initial release.
- Core entity loading and writing functionality.
- Support for both "Inside" (directory-based) and "Parallel" (file-based) entity storage.
- Filesystem abstraction via the `Xfs` trait.
