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
