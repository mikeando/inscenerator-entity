# Missing XFS Features

The following features are required by `inscenerator-entity` but are currently missing from the `inscenerator-xfs` crate:

1. `remove_file(path)`: Delete a single file.
2. `remove_dir_all(path)`: Delete a directory and all its contents.
3. `rename(from, to)`: Rename/move a file or directory.

These should be added to the `Xfs` trait and implemented for `OsFs` and `MockFS`.
