# setup environment variables
. ./environment.setup

########################################################################
##                              ZLIB
########################################################################

VERSION=1.2.8
PACKAGE="zlib-$VERSION"
FILE="$PACKAGE.tar.xz"
CHECKSUM='831df043236df8e9a7667b9e3bb37e1fcb1220a0f163b6de2626774b9590d057'

if [ -d $PREFIX/$PACKAGE ]; then
    echo "zlib version $VERSION already installed"
    exit 0
fi

echo "#### Downloading zlib ####"
wget -nc -O $FILE "http://zlib.net/$FILE"
echo "$CHECKSUM  $FILE" | sha256sum -c

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

tar xf "$FILE"
cd "$PACKAGE"

echo "#### Building zlib ####"
./configure --prefix="$PREFIX/$PACKAGE"
make -j $SLOTS

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

# Remove any previous installation dir
rm -rf "$PREFIX/$PACKAGE"

echo "#### Installing zlib ####"
make install

rm -f "$PREFIX/zlib-latest"
ln -s "$PREFIX/$PACKAGE" "$PREFIX/zlib-latest"

echo "#### Cleaning up environment ####"
cd ..
rm -rf "$PACKAGE" "$FILE"

exit 0
