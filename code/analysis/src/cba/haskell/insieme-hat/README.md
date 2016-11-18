# Putting a HAT on INSIEME

## Installation

The first step is to setup INSIEME's dependencies. This can be done by using the
provided installer inside `/scripts/dependencies`, take a look at the
corresponding `README.md`, as well as the main one located in the root directory
of the INSIEME project.

Besides Souffle (Datalog engine) the installer will download a pre-built binary
release of the Glasgow Haskell Compiler suite and build a development snapshot
of the `cabal` tool with support for foreign-libraries with it. After this
bootstrapping process has been completed, a usable `cabal` binary will be
located inside `$PREFIX/cabal-*/bin`.

Building INSIEME via Make will internally invoke `cabal new-build` to build the
`insieme-hat` Haskell package and produce a shared library which can then be
linked with the C/C++ part of the compiler. The environment variables `PATH`,
`LIBRARY_PATH` and `LD_LIBRARY_PATH` are automatically set before calling
`cabal` so the compiler can find GMP and zlib if they are installed through the
dependency installer.

You can run only the Haskell part of the build system by running:

    $ make insieme_hat

To use `cabal` or `ghc` directly you can use the `haskell-env.sh` script in the
root of the CMake build directory. For example, assuming `BUILD_DIR` contains
the CMake build directory, the following will start a REPL for the project:

    $ sh "$BUILD_DIR"/haskell-env.sh cabal new-repl

When doing this please note that we override the `HOME` environment variable to
force `cabal` to place build products of dependencies in `BUILD_DIR`.

## Profiling

Profiling is not supported for the foreign library interface at the moment. One
can use the `cba_runner` executable for this purpose though.

To build insieme-hat using profiling the `-DANALYSIS_HASKELL_PROFILING=ON` CMake
option must be set. One can use a fresh build directory for the profiling build
if desired but this is not mandatory.

To generate input files for `cba_runner` the `haskell_dumper` driver is
required, to build it one must first build insieme. Assuming the
`haskell_dumper` is located in one's path one may use the following commands to
issue a profiling run:

    $ haskell_dumper -i /path/to/cba_test.c -d cba_test.irbh
    $ ./scripts/hat-exec "$BUILD_DIR" cba_runner --RTS +RTS -p < cba_test.irbh
    
where `$BUILD_DIR` is the path to the CMake build directory. 

After running these commands there should be a text file named `cba_runner.prof`
in the current directory containg the profiling information.

When profiling has been enabled, one can also pass the `-xc` Haskell-RTS option
to print a stack trace upon encouenring an exception.

    $ ./scripts/hat-exec "$BUILD_DIR" cba_runner -- --RTS +RTS -xc < cba_test.irbh

### Generating Reports

After you have obtained a `*.prof` file you can generate some pretty graphs
using the `scripts/profiling/report.sh` script. Before you can use it you must
have built `insieme-hat` using the CMake build system as it requires a Haskell
program to generate the graphs.

    $ ./scripts/profiling/report.sh "$BUILD_DIR" my_fancy_report/ cba_runner.prof

## Testing via C/C++

Tests located in the analysis part of INSIEME also include unit-tests utilizing
the Haskell package via the adapter. The adapter builds a bridge between C/C++
and Haskell via Haskell's foreign function interface (ffi).

## Documentation

If the CMake option `-DBUILD_DOCS=ON` is set Haddock documentation for all
exposed modules in `insieme-hat` will be built.

