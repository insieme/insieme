#!/bin/sh
set -x
cd "$(dirname "$0")"

env \
    PATH="${INSIEME_LIBS_HOME}/cabal-latest/bin/:${INSIEME_LIBS_HOME}/ghc-latest/bin/:$PATH" \
    LIBRARY_PATH="${INSIEME_LIBS_HOME}/gmp-latest/lib:${INSIEME_LIBS_HOME}/zlib-latest/lib" \
    LD_LIBRARY_PATH="${INSIEME_LIBS_HOME}/gmp-latest/lib:${INSIEME_LIBS_HOME}/zlib-latest/lib" \
    HOME="$BUILDDIR/code/analysis/src/cba/haskell/insieme-hat/cabal-home" \
    sh -x <<EOF
cabal update
cabal new-build -j1 -v2
EOF
