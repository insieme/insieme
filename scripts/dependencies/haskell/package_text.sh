NAME="text"
VERSION="1.2.3.0"
PACKAGE="$NAME-$VERSION"

FILE="${PACKAGE}.tar.gz"
URL="https://hackage.haskell.org/package/${PACKAGE}/${PACKAGE}.tar.gz"
REVISION_CABAL=0
URL_CABAL="https://hackage.haskell.org/package/${PACKAGE}/revision/${REVISION_CABAL}.cabal"

SHA256SUM="20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a"
SHA256SUM_CABAL="9dc17a9736056785e11d8a6e3e1e3a88b2a16ed78bf6f27d34ff0827bad1ca21"

DEPENDS="zlib gmp ghc gcc"
