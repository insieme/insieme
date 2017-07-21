#!/bin/sh

set -e

usage () {
    echo "Usage: $0 CMAKE_BUILD_DIR REPORT_DIR FILE.prof" >&2
}

if [ $# -lt 3 ]; then
    usage
    exit 1
fi

builddir="$(realpath "$1")"; shift
outdir="$(realpath "$1")"; shift
prof="$(realpath "$1")"; shift

cd "$(dirname "$0")"

mkdir -p "$outdir"

for ty in entries ticks bytes; do
    ( cd ../..; ./utils/hat-exec "$cmake_builddir" profiler flamegraph $ty "$prof" ) \
        | ./flamegraph.pl > "$outdir"/flamegraph-$ty.svg
    ( cd ../..; ./utils/hat-exec "$cmake_builddir" profiler callgraph $ty "$prof" ) \
        | dot -Tpdf -o "$outdir"/callgraph-$ty.pdf
done
