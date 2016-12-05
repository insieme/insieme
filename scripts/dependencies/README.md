# Dependencies Installer

These scripts ease the environment setup procedure required to build this
project.  Each file with the prefix `package_` contains meta data and build
instructions for a specific package as well as its dependencies. A default set
of instructions is inherited from `defaults.sh`, but can be overwritten by each
package.

The `installer` can be used to install packages, dependencies will be resolved
automatically. Simply provide the name of the packages which should be
installed as arguments to the installer.

    $ ./installer gcc ruby valgrind

The default `PREFIX` is set in `defaults.sh` and can be overwritten via an
environment variable:

    $ PREFIX=/opt/custom-libs ./installer gcc ruby valgrind

## Patches

The default `pkg_prepare` action will apply all patches inside the `patches`
directory which are prefixed with the package name. The order is inferred from
the filename which should have the following structure:

    <package name>-<patch number>-<comment>.patch

Examples:

    boost-0001-regex-fix.patch
    llvm-0001-insieme-clang.patch
    llvm-0002-fix-typos.patch

## Third Party Sym-Linker

All packages installed by the installer are separated by their name and
version, this allows one to have multiple versions of a package installed
side-by-side. Hence the content of `PREFIX` can be shared across multiple
projects.

In order for a project to find its required dependencies another layer of
indirection has to be added. 

Each project should contain a `third_party` folder inside the project's root
directory. This folder should contain symlinks, one for each dependency,
pointing to the installed package in `PREFIX`. The `third_party` folder should
not contain different versions of the same package. Example:

    $ PREFIX="$HOME/libs"
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
    $ ls -l MyAwesomeProject/third_party
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 autoconf -> /home/alex/libs/autoconf-2.68/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 automake -> /home/alex/libs/automake-1.15/
    lrwxrwxrwx. 1 alex dps   47 Nov 22 13:17 binutils -> /home/alex/libs/binutils-2.27/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 bison -> /home/alex/libs/bison-3.0.4/
    lrwxrwxrwx. 1 alex dps   46 Nov 22 13:17 boost -> /home/alex/libs/boost-1.59.0/
    lrwxrwxrwx. 1 alex dps   45 Nov 22 13:17 cmake -> /home/alex/libs/cmake-3.2.1/
        ...

The `third_party_linker` can create these symlinks for you, it uses the same
version of a package as defined in the related `package_` file.

## Custom GCC

If you chose to use GCC provided by the installer, `PATH` and `LD_LIBRARY_PATH`
should be set accordingly.

    export PATH="$PREFIX/gcc-$GCC_VERSION/bin:$PATH"
    export LD_LIBRARY_PATH="$PREFIX/gcc-$GCC_VERSION/lib64"

## Use custom GCC for dependency Installation

After installing GCC via this installer, open `default.sh` with an editor and
set `CC`, `CXX`, `PATH` and `LD_LIBRARY_PATH` to the newly installed GCC.
Continue the installation process.
