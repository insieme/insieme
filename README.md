# Insieme: Compiler and Runtime Infrastructure

The Insieme compiler is a source-to-source compiler for C/C++ that supports
portable parallel abstractions (Cilk, OpenMP and [AllScale API]) for
heterogeneous multi-core architectures. See our [mission statement] for more
details.

[AllScale API]: <http://www.allscale.eu/>
[mission statement]: <http://insieme-compiler.org/mission.html>

## Directory Structure

Insieme contains 4 main sub-directories:

| Directory  | Contains                            |
| ---------- | ----------------------------------- |
| `/code`    | Compiler and runtime implementation |
| `/cmake`   | CMake modules                       |
| `/docs`    | Developer documentation             |
| `/scripts` | Utility scripts                     |
| `/test`    | Integration tests                   |

## Installation

### Dependencies

A list of dependencies together with an installer is provided inside
`/scripts/dependencies`. See the [dependency README] for more information about
this setup process.

[dependency README]: <scripts/dependencies/README.md>

For a quickstart, run these two scripts

    $ scripts/dependencies/installer boost
    $ scripts/dependencies/third_party_linker

### Configuration

Following options can be supplied to CMake:

| Option                         | Values          |
| ------------------------------ | --------------- |
| -DCMAKE_BUILD_TYPE             | Release / Debug |
| -DBUILD_SHARED_LIBS            | ON / OFF        |
| -DBUILD_TESTS                  | ON / OFF        |
| -DBUILD_DOCS                   | ON / OFF        |
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

### PAPI

If PAPI is used, you have to add PAPI's lib directory to your `LD_LIBRARY_PATH`.

    $ export LD_LIBRARY_PATH="$INSIEME_SRC/third_party/papi/lib:$LD_LIBRARY_PATH"

### OpenCL

Currently OpenCL is required but not provided by the dependency installer. You
are required to install it by hand (for now) and point CMake to the installed
location.

### GCC

The dependency installer provides a version of GCC suitable for Insieme, since
some distros still ship a version of GCC that is too old for us to work with.
`CC`, `CXX`, `PATH` and `LD_LIBRARY_PATH` should be set accordingly when
building Insieme. We assume `INSIEME_SRC` contains the path to the Insieme
source directory, although it is not necessary to set this environment
variable.

    $ export CC="$INSIEME_SRC/third_party/gcc/bin/gcc"
    $ export CXX="$INSIEME_SRC/third_party/gcc/bin/g++"
    $ export PATH="$INSIEME_SRC/third_party/gcc/bin:$PATH"
    $ export LD_LIBRARY_PATH="$INSIEME_SRC/third_party/gcc/lib64"

### Building Insieme

You can now setup a build directory using CMake:

    $ mkdir build
    $ cd build
    $ $INSIEME_SRC/third_party/cmake/bin/cmake -DOPENCL_ROOT=/path/to/opencl $INSIEME_SRC

If successful, CMake produces the set of Makefiles required to build the
Insieme project. Following command builds Insieme.

    $ make -j8              # Builds using 8 parallel jobs

### Analysis Framework

The analysis framework is divided into a common interface and multiple,
different engines. The common interface has to be instantiated with one of the
available engines in order to use it. These engines are disabled by default
since they depend on additional third-party packages. They can be enabled by
passing their respective flags to the `cmake` call.

Also see the [dependency README] inside `/scripts/dependencies`.

For some analysis engines additional paths must be provided to CMake

| Analysis Engine | Required Path                       |
| --------------- | ----------------------------------- |
| Datalog         | -DSOUFFLE_ROOT=/path/to/souffle     |
| Haskell         | -DSTACK_ROOT=/path/to/haskell_stack |

### Running Unit Tests

After the actual build process has been completed, the `integration_tests`
binary will be executed automatically to setup the environment for certain
unit- and integration tests. This can also be achieved manually be running the
binary with the `--preprocessing` flag.

    $ code/driver/integration_tests --preprocessing

Unit tests can then be executed via ctest

    $ $INSIEME_SRC/third_party/cmake/bin/ctest -j8       # Runs all unit tests using 8 parallel jobs

### Running Integration Tests

Integration tests can be executed using the custom runner compiled in the build
directory:

    $ code/driver/integration_tests

Integration tests can be executed in parallel (`-w SLOTS`), and multiple times
(`-r N`). For a full list of options use the `-h` argument or refer to the
Insieme developer documentation. The mock run (`-m`) will give you an idea of
the actual commands being invoked.

To clean up files generated in the preprocessing step, simply run:

    $ code/driver/integration_tests --postprocessing

If everything was successful... **congratulations! You can start enjoying
Insieme now!**

### Compiling Application Codes

The main executable provided by the Insieme framework is called `insiemecc`,
located in `/code/driver`. It can be used to replace e.g. occurrences of
another compiler such as `gcc` in makefiles. It supports both
source-to-source-only compilation, as well as full compilation by calling a
backend compiler.  Environment variables `INSIEME_C_BACKEND_COMPILER` and
`INSIEME_CXX_BACKEND_COMPILER` can be used to change the backend compiler at
runtime, while the CMake options `-DINSIEME_C_BACKEND_COMPILER` and
`-DINSIEME_CXX_BACKEND_COMPILER` allow setting the compiler prior building.
`gcc` and `g++` are used as default. For further information on its features
and options, please refer to:

    $ code/driver/insiemecc --help

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
