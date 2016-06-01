#!/bin/bash

set -e

srcdir="${1}"
outdir="${2}"
incdir="include"

# Check if in and out dirs are given and exist
if [ "x$srcdir" == "x" ]; then
	echo "Please provide a source dir as first argument!"
	exit 1
elif [ "x$outdir" == "x" ]; then
	echo "Please provide an output dir as second argument!"
	exit 1
elif [ ! -d "$srcdir" ]; then
	echo "Given source dir '$srcdir' does not exist!"
	exit 1
elif [ ! -d "$outdir" ]; then
	echo "Given output dir '$outdir' does not exist!"
	exit 1
elif [ ! -d "$srcdir/$incdir" ]; then
	echo "Given include dir '$incdir' does not exist in '$srcdir'!"
	exit 1
fi

cp -r "$srcdir/"*".dl" "$srcdir/$incdir" "$outdir"

