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

    $ PREFIX=/opt/insieme-libs ./installer

## List of Required Libraries and Software

Insieme is written in C++11 and relies on several third-party libraries:

| Name          | Version   | Purpose                                                             |
| ------------- | --------- | ------------------------------------------------------------------- |
| [G++]         | >= 5.1    | Compiler                                                            |
| [CMake]       | >= 3.2.x  | Build System                                                        |
| [Google Test] | >= 1.8    | Unit testing --- is installed/built by us --- no need to install it |
| [Boost]       | >= 1.59   | Utilities, regex, filesystem, program options, ...                  |
| [LLVM/Clang]  | = 3.6.2\* | C/C++ frontend                                                      |
| [CUDD]        | >= 2.4.2  | Manipulation of decision diagrams                                   |
| [LuaJIT]      | >= 2.0.0  | Scripting                                                           |
| [Bison]       | >= 3.0    | Inspire language parser                                             |
| [Flex]        | >= 2.5    | Inspire language scanner                                            |
| [Ruby]        | >= 2.0    | Scripting                                                           |

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

| Name       | Version  | Purpose                                                     |
| ---------- | -------- | ----------------------------------------------------------- |
| [PAPI]     | >= 5.4.0 | Runtime system, for hardware information and event counters |
| [hwloc]    | >= 1.10  | Runtime system, for system architecture information         |
| [valgrind] | >= 3.11  | Memory checks                                               |
| [Soufflé]  | \*       | Datalog Analsysis Engine                                    |
| [Stack]    | 1.0.4\*  | Haskell Analsysis Engine                                    |

\* Use Insieme's dependency installer

[PAPI]: <http://icl.cs.utk.edu/papi/>
[hwloc]: <http://www.open-mpi.org/projects/hwloc/>
[valgrind]: <http://valgrind.org/>

## Patches

The default `pkg_prepare` action will apply all patches inside the `patches`
directory which are prefixed with the package name. The order is inferred from
the filename.

## Paths

`INSIEME_LIBS_HOME` should be set to the prefix of the installer.

    export INSIEME_LIBS_HOME="$HOME/libs"

If you chose to use GCC provided by the installer, `PATH` and `LD_LIBRARY_PATH`
should be set accordingly:

    export PATH="$INSIEME_LIBS_HOME/gcc-latest/bin:$PATH"
    export LD_LIBRARY_PATH="$INSIEME_LIBS_HOME/gcc-latest/lib64"

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

    export PATH="$INSIEME_LIBS_HOME/ruby-latest/bin:$PATH"
    export PATH="$INSIEME_LIBS_HOME/valgrind-latest/bin:$PATH"
