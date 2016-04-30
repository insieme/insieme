# setup environment variables
. ./environment.setup

########################################################################
##                              CABAL
########################################################################

VERSION=1.22.9.0
PACKAGE="cabal-install-$VERSION"
PACKAGE_LOCAL="cabal-$VERSION"
FILE="$PACKAGE.tar.gz"
CHECKSUM='874035e5730263653c7aa459f270efbffc06da92ea0c828e09ebc04400e94940'

if [ -d $PREFIX/$PACKAGE ]; then
    echo "Cabal version $VERSION already installed"
    exit 0
fi

echo "#### Downloading Cabal ####"
wget -nc -O $FILE "https://hackage.haskell.org/package/${PACKAGE}/${PACKAGE}.tar.gz"
echo "$CHECKSUM  $FILE" | sha256sum -c

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

tar xf "$FILE"
cd "$PACKAGE"

# Remove any previous installation dir
rm -rf "$PREFIX/$PACKAGE_LOCAL" "$HOME/.cabal" "$HOME/.ghc"

echo "#### Installing Cabal ####"
PATH="$PREFIX/ghc-latest/bin:$PATH" LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib" PREFIX="$PREFIX/$PACKAGE_LOCAL" ./bootstrap.sh

RET=$?
if [ $RET -ne 0 ]; then
    exit $RET
fi

rm -f "$PREFIX/cabal-latest"
ln -s "$PREFIX/$PACKAGE_LOCAL" "$PREFIX/cabal-latest"

echo "#### Cleaning up environment ####"
cd ..
rm -rf "$PACKAGE" "$FILE"

echo "#### Populate package database ####"
"$PREFIX/cabal-latest/bin/cabal" update

exit 0
