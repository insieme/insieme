# Putting a HAT on INSIEME

## Installation

Your first thing to do is to setup INSIEME's dependencies. This can be done by
using the provided installer inside `/scripts/dependencies`, take a look at the
`README.md` inside there, as well as the main one located in the root directory
of the project.

Besides Souffle (Datalog engine) the installer will download a pre-built binary
of `stack` as well as `stack`'s source code, apply patches and build it using
the pre-built binary. After this bootstrapping process has been completed, a
(patched) `stack` binary will be located inside `$PREFIX/stack-latest/bin`.

Building INSIEME as usual will invoke `stack` to build this Haskell package,
produce a shared library which can then be linked with the C/C++ part. The
environment variables `STACK_ROOT`, `LIBRARY_PATH` and `LD_LIBRARY_PATH` are
set for each call so `stack` finds GMP and zlib and places downloaded
components inside `$PREFIX/stack-latest/stack-root_$USER`.

**Note: The user executing the build process (and hence `stack`) requires write
permissions to `$PREFIX/stack-latest/`.**

Following commands are used to build the Haskell package:

    $ stack setup           # Get the appropriate GHC for your project
    $ stack build alex      # Install alex binary
    $ stack build c2hs      # Install c2hs binary
    $ stack build           # Build the project (includes getting dependencies)

Building via CMAKE will pass the `-lHSrts-ghc7.10.3` flag to GHC so the Haskell
runtime environment will be linked. This is important when later linking
together the INSIEME analysis shared library. Assuming `INSIEME_LIBS_HOME` is
set accordingly, one can set following environment variables to use `stack` as
usual:

    export PATH="$INSIEME_LIBS_HOME/stack-latest/bin:$PATH"
    export STACK_ROOT="$INSIEME_LIBS_HOME/stack-latest/stack-root_$USER"
    export LIBRARY_PATH="$INSIEME_LIBS_HOME/gmp-latest/lib:$INSIEME_LIBS_HOME/zlib-latest/lib:$LIBRARY_PATH"
    export LD_LIBRARY_PATH="$INSIEME_LIBS_HOME/gmp-latest/lib:$INSIEME_LIBS_HOME/zlib-latest/lib:$LD_LIBRARY_PATH"

**Note: Certain paths and flags have to be updated when using different version
GHC.**

## Development Workflow

While the Haskell package can not be used as standalone (for now), it is not
necessary to compile the whole INSIEME project the work with this package.

Haskell specific tests can be run, as well as opening up GHCi via `stack`.
Starting GHCi will display some warnings / error caused by the adapter (and
ffi), these can usually be ignored since you won't use the adapter via GHCi.
The build process also produces a binary (entry point `src/Main.hs`) which can
be utilized for rapid experiments.

Setting `STACK_ROOT` to the same value as in `CMakeLists.txt` (given the same
user) prevents you from re-downloading all dependencies and build requirements
again.

## Testing via C/C++

Tests located in the analysis part of INSIEME also include unit-tests utilizing
the Haskell package via the adapter. The adapter builds a bridge between C/C++
and Haskell via Haskell's foreign function interface (ffi).

Relevant C++ objects like an IR tree or a `NodeAddress` can be passed over to
Haskell and vice versa. A pointer to the resulting Haskell object is returned,
which can be used as a handle inside the C/C++ part. This handle can be passed,
for instance, to one of the analysis function.

## Builiding Internal Documentation

While `stack` can run `haddock` on a whole package it only takes exposed
modules into account and there is no way to pass a `--internal` flag as you
could do with `cabal`. A simple workaround is to move all modules from the
`other-modules` list of the `library` section in `insieme-hat.cabal` to
`exposed-modules` and run `stack haddock --no-haddock-deps`. The resulting
files will be located inside `.stack-work/doc/.../insieme-hat`.

## Known Issues

### Package Hash in library name

The resulting library name contains an ABI Hash[^1]. Since we don't know the
hash during the CMake setup process a workaround has been introduced by copying
over the latest shared library from the `.stack-work` directory to the
`${CMAKE_CURRENT_BINARY_DIR}` under a constant name.

Unfortunately the original filename is present inside the shared library itself
and used during the linking stage. The solution to this was to copy the library
over twice (renamed and original). CMake refers to the renamed library
(constant name), while the linker will use the one with the hash in its name
referred to by the renamed one.

[^1]: see <https://www.vex.net/~trebla/haskell/sicp.xhtml>

### `stack build` .. `make`

When building the Haskell package manually with by issuing `stack build` the
Haskell runtime environment will not be linked. If one now builds INSIEME
without doing any changes to the Haskell source code, `stack` will not
recompile the library despite a different flag
(`--ghc-options='-lHSrts-ghc7.10.3'`) has been provided this time. This results
in a linking error later on.

Just insert a newline in any source file of the Haskell project and run `make`
again. `stack` should now rebuild the shared library. If this does not help,
remove the `.stack-work` folder and run `make`.
