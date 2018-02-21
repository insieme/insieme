NAME="parsec"
VERSION="3.1.13.0"
PACKAGE="$NAME-$VERSION"

FILE="${PACKAGE}.tar.gz"
URL="https://hackage.haskell.org/package/${PACKAGE}/${PACKAGE}.tar.gz"
REVISION_CABAL=0
URL_CABAL="https://hackage.haskell.org/package/${PACKAGE}/revision/${REVISION_CABAL}.cabal"

SHA256SUM="7861ae437a6177ee7c08899432fd8c062e7c110361da48a9f9e88263fd4d80f1"
SHA256SUM_CABAL="8dbd67e16adff703c48d69414f88bd86f59dccb1fdd509358905bdddfe09d705"

DEPENDS="zlib gmp ghc gcc  haskell/mtl haskell/text"
