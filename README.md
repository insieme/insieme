# Insieme: Compiler and Runtime Infrastructure

The Insieme compiler is a source-to-source compiler for C/C++ that supports
portable parallel abstractions (OpenMP, MPI and OpenCL) for heterogeneous
multi-core architectures. More details are available
[here](http://insieme-compiler.org/mission.html).

## *IMPORTANT NOTE*

The master branch of Insieme is currently undergoing severe changes and
refactoring. Until these changes are complete, several features will be missing
and all interfaces are in flux. If you just want to try Insieme as a user or
want to do research building on top of its infrastructure please use the
**inspire_1.3** branch.

## Directory Structure

Insieme contains 4 main sub-directories:

- `/code`    --- Insieme compiler implementation
- `/docs`    --- Insieme developer documentation
- `/scripts` --- Insieme utility scripts
- `/test`    --- Insieme integration tests

## Installation

### Dependencies

A list of dependencies together with an installer and patches is provided
inside `/scripts/dependencies`. This directory also houses a
[README](scripts/dependencies/README.md) which should be consulted when using
Insieme for the first time.

### Building Insieme

Insieme's build system is based on CMake. We assume that the environemnt is
setup in the directory pointed to by `$INSIEME_LIBS_HOME` using the provided
installer.

    $ export INSIEME_LIBS_HOME="$HOME/libs"

`CC`, `CXX`, `PATH` and `LD_LIBRARY_PATH` should be set when building Insieme
with newly installed GCC.

    $ export CC="$INSIEME_LIBS_HOME/gcc-latest/bin/gcc"
    $ export CXX="$INSIEME_LIBS_HOME/gcc-latest/bin/g++"
    $ export PATH="$INSIEME_LIBS_HOME/gcc-latest/bin:$PATH"
    $ export LD_LIBRARY_PATH="$INSIEME_LIBS_HOME/gcc-latest/lib64"

Additionally, `INSIEME_C_BACKEND_COMPILER` and
`INSIEME_CXX_BACKEND_COMPILER` should point to the desired source-to-binary
compiler. If not set, they default to `gcc` and `g++` in your `PATH`
respectively.

You can now setup a build directory using CMake:

    $ mkdir build
    $ cd build
    $ $INSIEME_LIBS_HOME/cmake-latest/bin/cmake ..

Some features of the compiler can be enabled / disabled by passing following
options (as argument) to the `cmake` command:

- Enable/disable generation of documentation
    - `-DDOCS=ON|OFF`
- Set the build to be in Debug, Release or Release with asserts mode
    - `-DCMAKE_BUILD_TYPE=Debug|Release|RelWithAsserts`
- Enable/disable energy measurement support
    - `-DUSE_ENERGY=ON|OFF`
- Enable/disable PAPI measurement and hardware information support
    - `-DUSE_PAPI=ON|OFF`
- Enable/disable valgrind memory checks for most unit tests
    - `-DCONDUCT_MEMORY_CHECKS=ON|OFF`
- Enable/disable building of the Insieme Runtime
    - `-DBUILD_RUNTIME=ON|OFF`
- Enable/disable building of the Insieme Compiler
    - `-DBUILD_COMPILER=ON|OFF`

If successful, CMake produces the set of Makefiles required to build the
Insieme project.

    $ make -j2              # Builds using two parallel jobs

Or you can alternatively change into a subfolder and only build that particular
module of the compiler.

    $ cd core
    $ make

### Analysis Framework

The analysis framework is divided into a common interface and multiple,
different engines. The common interface has to be instantiated with one of the
available engines in order to use it. These engines are disabled by default
since they depend on additional third-party packages. They can be enabled by
passing following flags to the `cmake` call.

| Engine  | CMake Option            |
|---------|-------------------------|
| Soufflé | `-DANALYSIS_DATALOG=ON` |
| Haskell | `-DANALYSIS_HASKELL=ON` |

Also see `/scripts/dependencies/README.md`.

### Running Unit Tests

For certain unit- and integration tests to run, the environment has to be
prepared (e.g. precompiling libraries to link against). This can be achieved by
executing the following command:

    $ ./code/driver/integration_tests --preprocessing

Unit tests can then be executed with:

    $ make test ARGS=-j2    # Runs all unit tests using two parallel jobs

or directly via CTest:

    $ ctest -j2             # Runs all unit tests using two parallel jobs

### Running Integration Tests

Integration tests can be executed using the custom runner compiled in the build
directory:

    $ ./code/driver/integration_tests

Integration tests can be executed in parallel (`-w SLOTS`), and multiple times
(`-r N`). For a full list of options use the `-h` argument or refer to the
Insieme developer documentation. The mock run (`-m`) will give you an idea of
the actual commands being invoked.

To clean up files generated in the preprocessing step, simply run:

    $ ./code/driver/integration_tests --postprocessing

If everything was successful... **congratulations! You can start enjoying
Insieme now!**

### Compiling Application Codes

The main executable provided by the Insieme framework is called `insiemecc`,
located in `code/driver`. It can be used to replace e.g. occurrences of another
compiler such as `gcc` in makefiles. It supports both source-to-source-only
compilation, as well as full compilation by calling a backend compiler.
Environment variables `INSIEME_C_BACKEND_COMPILER` and
`INSIEME_CXX_BACKEND_COMPILER` can be used to change the backend compiler at
runtime, while the CMake options `-DINSIEME_C_BACKEND_COMPILER` and
`-DINSIEME_CXX_BACKEND_COMPILER` allow setting the compiler prior building.
`gcc` and `g++` are used as fallback. For further information on its features
and options, please refer to:

    $ ./code/driver/insiemecc --help

### Installation

Please, understand that the install command is not implemented since this is an
on-going development framework. Instead of polluting your local setup, we
prefer to use Insieme from the build directory. Install scripts may be provided
in future releases.
