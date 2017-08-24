# Insieme: Compiler and Runtime Infrastructure

The Insieme compiler is a source-to-source compiler for C/C++ that supports
portable parallel abstractions (Cilk, OpenMP and [AllScale API]) for
heterogeneous multi-core architectures. See our [mission statement] for more
details.

[AllScale API]: <http://www.allscale.eu/>
[mission statement]: <http://insieme-compiler.org/mission.html>

## Quick Start Guide

To build Insieme, clone this repository and take a peek at `QUICKSTART`. You
can directly run execute it, given you are on a recent Ubuntu / Debian.

    $ git clone https://github.com/insieme/insieme
    $ cd insieme
    $ ./QUICKSTART    # uses sudo

If you are using a different distribution use your package manager to install
all dependencies available. A list of dependencies can be viewed in
[`/scripts/dependencies/README.md`](scripts/dependencies/README.md).

For software not available in your package manager (or the ones that require an
Insieme specific patch) use the provided dependency installer inside
`/scripts/dependencies`.

## Directory Structure

Insieme contains 5 main sub-directories:

| Directory  | Contains                            |
| ---------- | ----------------------------------- |
| `/code`    | Compiler and runtime implementation |
| `/cmake`   | CMake modules                       |
| `/docs`    | Developer documentation             |
| `/scripts` | Utility scripts                     |
| `/test`    | Integration tests                   |

Throughout this README the build directory is assumed to be `/build`.

## Advanced Settings

### Configuration

Following options can be supplied to CMake:

| Option                         | Values          |
| ------------------------------ | --------------- |
| -DCMAKE_BUILD_TYPE             | Release / Debug |
| -DBUILD_SHARED_LIBS            | ON / OFF        |
| -DBUILD_TESTS                  | ON / OFF        |
| -DBUILD_DOCS                   | ON / OFF        |
| -DBUILD_COVERAGE               | ON / OFF        |
| -DUSE_ASSERT                   | ON / OFF        |
| -DUSE_VALGRIND                 | ON / OFF        |
| -DBUILD_COMPILER               | ON / OFF        |
| -DBUILD_RUNTIME                | ON / OFF        |
| -DUSE_PAPI                     | ON / OFF        |
| -DUSE_ENERGY                   | ON / OFF        |
| -DANALYSIS_DATALOG             | ON / OFF        |
| -DANALYSIS_HASKELL             | ON / OFF        |
| -DINSIEME_C_BACKEND_COMPILER   | \<path\>        |
| -DINSIEME_CXX_BACKEND_COMPILER | \<path\>        |
| -DTHIRD_PARTY_DIR              | \<path\>        |

These settings are defined in the [build_settings] and [insieme_specific] CMake
module, located in the `/cmake` subdirectory.

[build_settings]: <cmake/build_settings.cmake>
[insieme_specific]: <cmake/insieme_specific.cmake>

### OpenCL

The backend is also capable of generating OpenCL code. In order to utilize this
feature, one must supply the `OPENCL_ROOT` path to CMake. It should point to
the directory containing the `include` and `lib64` directory of the OpenCL
installation.

    $ cd build
    $ cmake -DOPENCL_ROOT=/path/to/opencl ..

### GCC

The dependency installer provides a version of GCC suitable for Insieme, since
some distros still ship a version of GCC that is too old for us to work with.
`CC`, `CXX`, `PATH` and `LD_LIBRARY_PATH` should be set accordingly when
building Insieme. We assume `INSIEME_SRC` contains the path to the Insieme
source directory, although it is not necessary to set this environment
variable.

    $ export CC="$BUILD_DIR/third_party/gcc/bin/gcc"
    $ export CXX="$BUILD_DIR/third_party/gcc/bin/g++"
    $ export PATH="$BUILD_DIR/third_party/gcc/bin:$PATH"
    $ export LD_LIBRARY_PATH="$BUILD_DIR/third_party/gcc/lib64"

### Analysis Framework

The analysis framework is divided into a common interface and multiple,
different engines. The common interface has to be instantiated with one of the
available engines in order to use it. These engines are disabled by default
since they depend on additional third-party packages. They can be enabled by
passing their respective flags to the `cmake` call.

See [`/scripts/dependencies/README.md`](scripts/dependencies/README.md) for
information how to install their respective dependencies.

For some analysis engines additional paths must be provided to CMake:

| Analysis Engine | Required Path                                              |
| --------------- | ---------------------------------------------------------- |
| Datalog         | -DSOUFFLE_ROOT=/path/to/souffle/prefix                     |

### Running Tests

The build process will automatically build and run the `integration_tests`
binary with the `--preprocess` flag. This is required for some unit tests as
well as some integration tests.

Unit tests can be run via `ctest` as shown in the quick start guide:

    $ cd build
    $ ctest -j8

Integration tests can be run via the `integration_tests` binary.

    $ cd build
    $ driver/integration_tests -w 8

Furthermore this integration test driver provides useful features, like
repeating a test multiple times. See a full list of options by using `-h` as
argument.

Running the `integration_tests` binary with `--postprocessing` after all tests
have been completed successfully is recommended to clean up files generated in
the preprocessing setp.

If everything was successful...
**congratulations! You can start enjoying Insieme now!**

### Compiling Application Codes

The main executable provided by the Insieme framework is called `insiemecc`,
located in `build/driver`. It can be used to replace e.g. occurrences of
another compiler such as `gcc` in makefiles. It supports both
source-to-source-only compilation, as well as full compilation by calling a
backend compiler.  Environment variables `INSIEME_C_BACKEND_COMPILER` and
`INSIEME_CXX_BACKEND_COMPILER` can be used to change the backend compiler at
runtime, while the CMake options `-DINSIEME_C_BACKEND_COMPILER` and
`-DINSIEME_CXX_BACKEND_COMPILER` allow setting the compiler prior building.
`gcc` and `g++` are used as default. For further information on its features
and options, please refer to:

    $ cd build
    $ driver/insiemecc --help

### Installation

Please, understand that the install command is not implemented since this is an
on-going development framework. Instead of polluting your local setup, we
prefer to use Insieme from the build directory. Install scripts may be provided
in future releases.

## Development

### Executable Bit

When working on Windows via SMB share, consider setting following Git setting.

    $ git config core.filemode false

### Licensor

A script, together with a Git hook, is provided to automatically add a license
header to each source file upon commit. See `/scripts/license`.

### Visual Studio Solution

    $ cmake -G "Visual Studio 15 2017 Win64" -DBUILD_SHARED_LIBS=OFF Z:\path\to\project

Add path for third-party libraries when needed.

### Coverage

Building the coverage is currently only supported on Linux, as Perl and Bash
are required. To build and view the coverage set the corresponding CMake flag
to `ON` and run:

    $ make
    $ make coverage
    $ xdg-open coverage/index.html
