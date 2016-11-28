# %PROJECT%

Description goes here...

## Installation

### Dependencies

See `scripts/dependencies/README.md` for more details. Otherwise do

    $ scripts/dependencies/installer boost
    $ scripts/dependencies/libs_linker

### Configuration

Following options can be supplied to CMake

| Option              | Values          |
| ------------------- | --------------- |
| -DCMAKE_BUILD_TYPE  | Release / Debug |
| -DBUILD_SHARED_LIBS | ON / OFF        |
| -DBUILD_TESTS       | ON / OFF        |
| -DBUILD_DOCS        | ON / OFF        |
| -DUSE_ASSERT        | ON / OFF        |

### Building / Testing

    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ make test

## Development

### Executable Bit

When working on Windows via SMB share, consider setting following Git setting.

    $ git config core.filemode false

### Licensor

A script, together with a Git hook, is provided to automatically add a license
header to each source file upon commit. See `scripts/license`.
