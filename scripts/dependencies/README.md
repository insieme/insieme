# Insieme Environment Setup

These scripts ease the environment setup procedure required to build Insieme.
Each file with the prefix `package_` contains meta data and build instructions
for a specific package as well as its dependencies.

The `installer` can be used to install packages, dependencies will be resolved
automatically if `--with-depends` is provided.

    $ ./installer gmp mpfr mpc gcc

or

    $ ./installer --with-depends gcc ruby valgrind

Note that all dependencies will be installed regardless of whether they are
already present on your system, outside of this dependency installer.

The default `PREFIX` is set in `config.sh` and can be overwritten via an
environment variable:

    $ PREFIX=/opt/custom-libs ./installer

While `PREFIX` takes precedence, the environment variable `THIRD_PARTY_LIBS` is
also considered.

## Using Installed Packages

To use one of the installed packages directly adjust your `PATH` and/or
`LD_LIBRARY_PATH` accordingly.

## Patches

The default `pkg_prepare` action will apply all patches inside the `patches`
directory which are prefixed with the package name. The order is inferred from
the filename which should have the following structure:

    <package name>-<patch number>-<comment>.patch

## Older GCC

Some systems only provide a GCC version < 5. We therefore have to bootstrap a
newer GCC first. This can be done by enabling everything under *Override
Compiler* inside `config.sh`. Install GCC before installing other dependencies.

    $ ./installer gcc
    $ ./installer llvm cudd boost ...

## Third Party Symlinker

All packages installed by the installer are separated by their name and
version, this allows one to have multiple versions of a package installed
side-by-side. Yet, in order for a project to find its required dependencies
another layer of indirection is added.

A folder `third_party` should be located inside the Insieme build directory,
containing symlinks for each dependency. The symlinks point to
installed packages in `PREFIX`. The `third_party` folder should not contain
different versions of the same package. Example:

    $ PREFIX="$HOME/third_party_libs"
    $ ls -l $PREFIX
    drwxr-xr-x.  4 alex   dps 4.0K Nov 18 15:14 autoconf-2.68/
    drwxr-xr-x.  4 alex   dps 4.0K Nov 18 15:14 automake-1.15/
    drwxr-xr-x.  7 alex   dps 4.0K Nov 18 15:14 binutils-2.27/
    drwxr-xr-x.  5 alex   dps 4.0K Nov 18 15:14 bison-3.0.4/
    drwxr-xr-x.  4 alex   dps 4.0K Nov 18 15:14 boost-1.50.0/
    drwxr-xr-x.  4 alex   dps 4.0K Nov 18 15:14 boost-1.59.0/
    drwxr-xr-x.  5 alex   dps 4.0K Nov 18 15:14 cmake-3.2.1/
    drwxr-xr-x.  5 alex   dps 4.0K Nov 18 15:14 cmake-3.6.1/
        ...

    $ ls -l $INSIEME_SRC/build/third_party
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 autoconf -> /home/alex/third_party_libs/autoconf-2.68/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 automake -> /home/alex/third_party_libs/automake-1.15/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 binutils -> /home/alex/third_party_libs/binutils-2.27/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 bison -> /home/alex/third_party_libs/bison-3.0.4/
    lrwxrwxrwx. 1 alex dps   46 Nov 22 13:17 boost -> /home/alex/third_party_libs/boost-1.59.0/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 cmake -> /home/alex/third_party_libs/cmake-3.2.1/
        ...

The `third_party_linker` can create these symlinks for you, it uses the same
version of a package as defined in the related `package_` file. The folder
`third_party` is created in the current working directory when invoking
`third_party_linker`.

## List of Required Libraries and Software

Insieme is written in C++11 and relies on several third-party libraries:

| Name          | Version    | Purpose                                                             | License                |
| ------------- | ---------- | ------------------------------------------------------------------- | ---------------------- |
| [G++]         | >= 5.1     | Compiler                                                            | GPLv3                  |
| [CMake]       | >= 3.2.x   | Build System                                                        | BSD 3-clause License   |
| [Google Test] | >= 1.8     | Unit testing --- is installed/built by us --- no need to install it | New BSD License        |
| [Boost]       | >= 1.59    | Utilities, regex, filesystem, program options, ...                  | Boost Software License |
| [LLVM/Clang]  | = 3.6.2\*  | C/C++ frontend                                                      | LLVM Release License   |
| [CUDD]        | >= 2.4.2\* | Manipulation of decision diagrams                                   | see source             |
| [LuaJIT]      | >= 2.0.0   | Scripting                                                           | MIT License            |
| [Bison]       | >= 3.0     | Inspire language parser                                             | GPLv3                  |
| [Flex]        | >= 2.5     | Inspire language scanner                                            | modified BSD License   |
| [Ruby]        | >= 2.0     | Scripting                                                           | BSD 2-clause License   |

\* Insieme specific patch required.

[G++]: <http://gcc.gnu.org/gcc-5/>
[CMake]: <http://www.cmake.org/>
[Google Test]: <https://code.google.com/p/googletest/>
[Boost]: <http://www.boost.org/users/history/version_1_50_0.html>
[LLVM/Clang]: <http://llvm.org/>
[CUDD]: <http://vlsi.colorado.edu/~fabio/CUDD/>
[LuaJIT]: <http://luajit.org/>
[Bison]: <https://www.gnu.org/software/bison/>
[Flex]: <http://flex.sourceforge.net/>
[Ruby]: <http://www.ruby-lang.org/en/>

## List of Optional Libraries and Software

| Name       | Version  | Purpose                                                     | License              |
| ---------- | -------- | ----------------------------------------------------------- | -------------------- |
| [PAPI]     | >= 5.4.0 | Runtime system, for hardware information and event counters | see source           |
| [hwloc]    | >= 1.10  | Runtime system, for system architecture information         | New BSD License      |
| [valgrind] | >= 3.11  | Memory checks                                               | GPLv2                |
| [Soufflé]  | \*       | Datalog Analsysis Engine                                    | UPL                  |
| [GHC]      | 8.0.2    | Haskell Analsysis Engine                                    | 3-Clause BSD License |
| [cabal]    | 42d92c9\*| Haskell Analsysis Engine                                    | 3-Clause BSD License |

\* Use Insieme's dependency installer

[PAPI]: <http://icl.cs.utk.edu/papi/>
[hwloc]: <http://www.open-mpi.org/projects/hwloc/>
[valgrind]: <http://valgrind.org/>
[GHC]: <https://www.haskell.org/ghc/>
[cabal]: <https://www.haskell.org/cabal/>
