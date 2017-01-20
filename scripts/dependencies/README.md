# Insieme Environment Setup

These scripts ease the environment setup procedure required to build Insieme.
Each file with the prefix `package_` contains meta data and build instructions
for a specific package as well as its dependencies. A default set of
instructions is inherited from `defaults.sh`, but can be overwritten by each
package.

The `installer` can be used to install packages, dependencies will be resolved
automatically. Invoking the installer without arguments will install the
default set of packages needed to build and run Insieme on most systems.

    $ ./installer

You can also install only specific packages by stating their names. Their
dependencies will be installed too:

    $ ./installer gcc ruby valgrind

The default `PREFIX` is set in `defaults.sh` and can be overwritten via an
environment variable:

    $ PREFIX=/opt/custom-libs ./installer

## List of Required Libraries and Software

Insieme is written in C++11 and relies on several third-party libraries:

| Name          | Version   | Purpose                                                             | License                |
| ------------- | --------- | ------------------------------------------------------------------- | ---------------------- |
| [G++]         | >= 5.1    | Compiler                                                            | GPLv3                  |
| [CMake]       | >= 3.2.x  | Build System                                                        | BSD 3-clause License   |
| [Google Test] | >= 1.8    | Unit testing --- is installed/built by us --- no need to install it | New BSD License        |
| [Boost]       | >= 1.59   | Utilities, regex, filesystem, program options, ...                  | Boost Software License |
| [LLVM/Clang]  | = 3.6.2\* | C/C++ frontend                                                      | LLVM Release License   |
| [CUDD]        | >= 2.4.2  | Manipulation of decision diagrams                                   | see source             |
| [LuaJIT]      | >= 2.0.0  | Scripting                                                           | MIT License            |
| [Bison]       | >= 3.0    | Inspire language parser                                             | GPLv3                  |
| [Flex]        | >= 2.5    | Inspire language scanner                                            | modified BSD License   |
| [Ruby]        | >= 2.0    | Scripting                                                           | BSD 2-clause License   |

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
| [Stack]    | 1.0.4\*  | Haskell Analsysis Engine                                    | BSD 3-Clause License |

\* Use Insieme's dependency installer

[PAPI]: <http://icl.cs.utk.edu/papi/>
[hwloc]: <http://www.open-mpi.org/projects/hwloc/>
[valgrind]: <http://valgrind.org/>

## Patches

The default `pkg_prepare` action will apply all patches inside the `patches`
directory which are prefixed with the package name. The order is inferred from
the filename which should have the following structure:

    <package name>-<patch number>-<comment>.patch

## Third Party Sym-Linker

All packages installed by the installer are separated by their name and
version, this allows one to have multiple versions of a package installed
side-by-side. Hence the content of `PREFIX` can be shared across multiple
projects.

In order for a project to find its required dependencies another layer of
indirection has to be added.

A folder `third_party` should be located inside the Insieme source directory,
containing contain symlinks for each dependency. The symlink points to the
installed package in `PREFIX`. The `third_party` folder should not contain
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

    $ ls -l $INSIEME_SRC/third_party
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 autoconf -> /home/alex/third_party_libs/autoconf-2.68/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 automake -> /home/alex/third_party_libs/automake-1.15/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 binutils -> /home/alex/third_party_libs/binutils-2.27/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 bison -> /home/alex/third_party_libs/bison-3.0.4/
    lrwxrwxrwx. 1 alex dps   46 Nov 22 13:17 boost -> /home/alex/third_party_libs/boost-1.59.0/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 cmake -> /home/alex/third_party_libs/cmake-3.2.1/
        ...

The `third_party_linker` can create these symlinks for you, it uses the same
version of a package as defined in the related `package_` file.

## Ubuntu 16.04 LTS

Environment setup has been tested on a clean Ubuntu 16.04 Server (amd64)
installation:

    $ sudo apt-get update
    $ sudo apt-get install build-essential m4 pkg-config python ruby unzip valgrind
    $ ./installer

## CentOS 6

A setup on an older version of CentOS (Minimal) has been tested too.

    $ su -c 'yum update'
    $ su -c 'yum groupinstall development'
    $ su -c 'yum install ruby time valgrind wget'

Since CentOS 6 still uses GCC 4, we have to first install GCC 5. All following
packages can be compiled with the newly installed GCC 5 by setting `CC`, `CXX`,
`PATH` and `LD_LIBRARY_PATH` inside `defaults.sh`.

    $ ./installer gcc
    $ vi defaults.sh
    $ ./installer

`CC`, `CXX`, `PATH` and `LD_LIBRARY_PATH` should be set when building Insieme
with newly installed GCC.

## Additional packages

Some packages like Ruby and Valgrind are not installed by default since most
distributions have an up-to-date version in their repository. If this is not
the case, the provided installer can be used to install an more recent version
which will work for Insieme.

Adjust your environment variables as needed, for example:

    export PATH="$INSIEME_SRC/third_party/ruby/bin:$PATH"
    export PATH="$INSIEME_SRC/third_party/valgrind/bin:$PATH"
