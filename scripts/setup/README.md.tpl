# %PROJECT%

Description goes here...

## Dependencies

See `scripts/dependencies/README.md` for more details. Otherwise do

    $ scripts/dependencies/installer boost
    $ scripts/dependencies/libs_linker

## Configuration

Following options can be supplied to CMake

| Option              | Values          |
| ------------------- | --------------- |
| -DCMAKE_BUILD_TYPE  | Release / Debug |
| -DBUILD_SHARED_LIBS | ON / OFF        |
| -DBUILD_TESTS       | ON / OFF        |
| -DBUILD_DOCS        | ON / OFF        |
| -DUSE_ASSERT        | ON / OFF        |

## Building

    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
