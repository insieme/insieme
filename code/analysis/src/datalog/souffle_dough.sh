#!/bin/bash

set -e

srcdir="${1}"
outdir="${2}"
incdir="include"

mangletoken="D474L06_"
decltoken="decl"

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

# Step 1: Name-mangle declarations in header files
cd "$outdir/$incdir"
for header in *.dl
do
	# Skip ir.dl
	test "$header" == "ir.dl" && continue;

	prefix="$(echo $mangletoken$header | sed 's/\./_/g')__"
	decls="$(cat "$header" | sed -n 's/^[[:space:]]*\.'$decltoken'[[:space:]]\+\([a-zA-Z0-9_]\+\)[[:space:]]*(.*)\?.*/\1/p')"
	new_header="$(cat "$header")"

	for decl in $decls
	do
		new_decl="$prefix$decl"
		new_header="$(echo "$new_header" | sed 's/\([^a-zA-Z0-9_]\+\|^\)'$decl'\([^a-zA-Z0-9_]\)/\1'$new_decl'\2/g')"
	done

	echo "$new_header" > $header
done
cd -
