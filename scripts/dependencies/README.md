# Insieme Environment Setup Scripts

These scripts ease the environment setup procedure required to build Insieme.
Each file with the prefix `package_` contains meta data and build instructions
for a specific package as well as its dependencies. A default set of
instructions is inherited from `defaults.sh`, but can be overwritten by each
package.

The `installer` can be used to install packages, dependencies will be resolved
automatically. Only packages required for the default build of Insieme will be
installed when no argument is provided:

    $ ./installer

You can also install only specific packages by stating their names:

    $ ./installer gcc ruby valgrind

The default `PREFIX` is set in `defaults.sh` and can be overwritten via an
environment variable:

    $ PREFIX=/opt/insieme-libs ./installer

## Patches

The default `pkg_prepare` action will apply all patches inside the `patches`
directory which are prefixed with the package name. The order is inferred from
the filename.

## Paths

`INSIEME_LIBS_HOME` should be set to the prefix of the installer and the newly
installed GCC should be used via `PATH` and `LD_LIBRARY_PATH`.

    export INSIEME_LIBS_HOME="/software-local/insieme-libs"
    export PATH="$INSIEME_LIBS_HOME/gcc-latest/bin:$PATH"
    export LD_LIBRARY_PATH="$INSIEME_LIBS_HOME/gcc-latest/lib64"

## Ubuntu 16.04 LTS

Environment setup has been tested on a clean Ubuntu 16.04 Server (amd64)
installation:

    $ sudo apt-get update
    $ sudo apt-get install build-essential m4 pkg-config python ruby unzip valgrind
    $ ./installer

## Ruby / Valgrind

Ruby and Valgrind can be installed via this installer, just in case your local
installation is too old.  Double check if some tests fail for you.

    $ ./installer ruby valgrind

Do not forget to add the new `bin` directory to your path.

    export PATH="$INSIEME_LIBS_HOME/ruby-latest/bin:$PATH"
    export PATH="$INSIEME_LIBS_HOME/valgrind-latest/bin:$PATH"
