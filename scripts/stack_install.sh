# setup environment variables
. ./environment.setup

set -e

stack_tmp() {
    STACK_ROOT="$TMP/stack-root" \
    C_INCLUDE_PATH="$PREFIX/gmp-latest/include:$PREFIX/zlib-latest/include" \
    LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib" \
    LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib" \
    "$TMP/stack" "$@"
}

########################################################################
##                              STACK (prebuilt)
########################################################################

VERSION=1.0.4
PACKAGE="stack-$VERSION"
FILE="$PACKAGE-linux-x86_64.tar.gz"
CHECKSUM="fedf161622170801f29be5d5096ea30e253b2bba54f185607f568c44ee151e5a"

if [ -d "$PREFIX/$PACKAGE" ]; then
    echo "Stack version $VERSION already installed"
    exit 0
fi

# create temporary folder
OLDWD=$(pwd)
TMP=$(mktemp -d --tmpdir stackinstallXXXX)
cd "$TMP"

echo "#### Downloading Stack (prebuilt) ####"
wget -nc -O $FILE "https://github.com/commercialhaskell/stack/releases/download/v$VERSION/$FILE"
echo "$CHECKSUM  $FILE" | sha256sum -c

echo "#### Unpacking Stack (prebuilt) ####"
tar xf "$FILE"
cp "$PACKAGE-linux-x86_64/stack" stack

echo "#### Cleaning up environment (prebuilt) ####"
rm -r "$FILE" "$PACKAGE-linux-x86_64"

########################################################################
##                              STACK (patched)
########################################################################

FILE="v$VERSION.tar.gz"
CHECKSUM="60df5eaeccd9db7fdb535f056815c9ec196731231d4754d2e294f74bef3f4547"

echo "#### Downloading Stack (patched) ####"
wget -nc -O $FILE "https://github.com/commercialhaskell/stack/archive/$FILE"
echo "$CHECKSUM  $FILE" | sha256sum -c

tar xf "$FILE"
cd "$PACKAGE"

echo "#### Applying patch ####"
patch -p1 << EOF
--- a/src/Stack/Types/Build.hs
+++ b/src/Stack/Types/Build.hs
@@ -606,6 +606,7 @@ configureOptsNoDir :: EnvConfig
                    -> [String]
 configureOptsNoDir econfig bco deps wanted isLocal package = concat
     [ depOptions
+    , ["--disable-library-stripping"]
     , ["--enable-library-profiling" | boptsLibProfile bopts || boptsExeProfile bopts]
     , ["--enable-executable-profiling" | boptsExeProfile bopts && isLocal]
     , ["--enable-split-objs" | boptsSplitObjs bopts]
EOF

echo "#### Building Stack (patched) ####"
stack_tmp setup
stack_tmp build

echo "#### Installing Stack (patched) ####"
mkdir -p "$PREFIX/$PACKAGE/bin"
cp "$(find .stack-work/install -name stack)" "$PREFIX/$PACKAGE/bin/stack"
ln -s "$PREFIX/$PACKAGE" "$PREFIX/stack-latest"

echo "#### Cleaning up environment (patched) ####"
cd "$OLDWD"
rm -rf "$TMP"

exit 0
