# %PROJECT%

Description goes here...

## Installation

### Dependencies

See `scripts/dependencies/README.md` for more details. Otherwise do

    $ scripts/dependencies/installer boost
    $ scripts/dependencies/third_party_linker

### Configuration

Following options can be supplied to CMake

| Option              | Values          |
| ------------------- | --------------- |
| -DCMAKE_BUILD_TYPE  | Release / Debug |
| -DBUILD_SHARED_LIBS | ON / OFF        |
| -DBUILD_TESTS       | ON / OFF        |
| -DBUILD_DOCS        | ON / OFF        |
| -DUSE_ASSERT        | ON / OFF        |
| -DUSE_VALGRIND      | ON / OFF        |
| -DTHIRD_PARTY_DIR   | \<path\>        |

The file `cmake/build_settings.cmake` states their default value.

### Building / Testing

    $ mkdir build
    $ cd build
    $ cmake ../code
    $ make -j8
    $ ctest -j8

## Development

### Adding new Modules

The setup script can be run again to add new modules, just provide the same
project name.

    $ scripts/setup/run %PROJECT% frontend backend utils

### Adding new Parts to Modules

There is a utility script to add new *parts* to an existing module. The project
name and module name must be provided followed by a list of *parts* to
generate. Folders will be created along the way.

    $ scripts/setup/add_part %PROJECT% frontend sema extensions/malloc_extension

This will add the files `sema.h`, `sema.cpp` and `sema_test.cc` to the
*frontend* module. Furthermore new subfolders `extensions` will be created
containing `malloc_extension.h`, `malloc_extension.cpp` and
`malloc_extension_test.cc` in their respective subdirectories.

### Executable Bit

When working on Windows via SMB share, consider setting following Git setting.

    $ git config core.filemode false

### Licensor

A script, together with a Git hook, is provided to automatically add a license
header to each source file upon commit. See `scripts/license`.

### Visual Studio Solution

    $ cmake -G "Visual Studio 14 Win64" -DBUILD_SHARED_LIBS=OFF Z:\path\to\project

Add path for third-party libraries when needed.
