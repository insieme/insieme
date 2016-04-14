# setup environment variables
. ./environment.setup

stack() {
    LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib" \
    LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib" \
    "$PREFIX/stack-latest/bin/stack" "$@"
}

########################################################################
##                              STACK (prebuilt)
########################################################################

VERSION=1.0.4
PACKAGE="stack-$VERSION"
FILE="$PACKAGE-linux-x86_64.tar.gz"
CHECKSUM="fedf161622170801f29be5d5096ea30e253b2bba54f185607f568c44ee151e5a"

if [ -d $PREFIX/$PACKAGE ]; then
    echo "Stack version $VERSION already installed"
    exit 0
fi

echo "#### Downloading Stack (prebuilt) ####"
wget -nc -O $FILE "https://github.com/commercialhaskell/stack/releases/download/v$VERSION/$FILE"
echo "$CHECKSUM  $FILE" | sha256sum -c

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

tar xf "$FILE"
cd "$PACKAGE-linux-x86_64"

# Remove any previous installation dir
rm -rf "$PREFIX/$PACKAGE" "$HOME/.stack"

echo "#### Installing Stack (prebuilt) ####"
mkdir -p "$PREFIX/$PACKAGE/bin"
cp stack "$PREFIX/$PACKAGE/bin/stack"

rm -f "$PREFIX/stack-latest"
ln -s "$PREFIX/$PACKAGE" "$PREFIX/stack-latest"

echo "#### Cleaning up environment (prebuilt) ####"
cd ..
rm -rf "$PACKAGE-linux-x86_64" "$FILE"

########################################################################
##                              STACK (patched)
########################################################################

FILE="v$VERSION.tar.gz"
CHECKSUM="60df5eaeccd9db7fdb535f056815c9ec196731231d4754d2e294f74bef3f4547"

echo "#### Downloading Stack (patched) ####"
wget -nc -O $FILE "https://github.com/commercialhaskell/stack/archive/$FILE"
echo "$CHECKSUM  $FILE" | sha256sum -c

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

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

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

echo "#### Building Stack (patched) ####"
stack setup

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

stack build

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

echo "#### Installing Stack (patched) ####"
cp "$(find .stack-work/install -name stack)" "$PREFIX/$PACKAGE/bin/stack"

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

echo "#### Cleaning up environment (patched) ####"
cd ..
rm -rf "$PACKAGE" "$FILE"

exit 0
