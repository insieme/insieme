# setup environment variables
. ./environment.setup

########################################################################
##                              GHC
########################################################################

VERSION=7.10.3
PACKAGE="ghc-$VERSION"
FILE="$PACKAGE.tar.xz"
CHECKSUM='b478e282afbf489614d0133ef698ba44e901eeb1794f4453c0fb0807cd271b96'

if [ -d $PREFIX/$PACKAGE ]; then
    echo "GHC version $VERSION already installed"
    exit 0
fi

echo "#### Downloading GHC ####"
wget -nc -O $FILE "http://downloads.haskell.org/~ghc/${VERSION}/${PACKAGE}-x86_64-deb8-linux.tar.xz"
echo "$CHECKSUM  $FILE" | sha256sum -c

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

tar xf "$FILE"
cd "$PACKAGE"

export LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH"

echo "#### Building GHC ####"
./configure --prefix="$PREFIX/$PACKAGE"

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

# Remove any previous installation dir
rm -rf "$PREFIX/$PACKAGE"

echo "#### Installing GHC ####"
make install

rm -f "$PREFIX/ghc-latest"
ln -s "$PREFIX/$PACKAGE" "$PREFIX/ghc-latest"

echo "#### Cleaning up environment ####"
cd ..
rm -rf "$PACKAGE" "$FILE"

exit 0
