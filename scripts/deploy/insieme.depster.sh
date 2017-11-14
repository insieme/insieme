#!/bin/sh
set -e -u

steps () {
    printf '%s\n' system-packages=root,cached build=cached
}

step () {
    local srcdir
    readonly srcdir="$PWD"/sourcedir
    cd "$srcdir"

    case "$1" in
	(system-packages)
	    ./scripts/environment/install_system_packages
	    ;;

	(build)
	    ./QUICKSTART
	    ;;
    esac
}

"$@"
