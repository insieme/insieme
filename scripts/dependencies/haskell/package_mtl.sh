NAME="mtl"
VERSION="2.2.1"
PACKAGE="$NAME-$VERSION"

FILE="${PACKAGE}.tar.gz"
URL="https://hackage.haskell.org/package/${PACKAGE}/${PACKAGE}.tar.gz"
REVISION_CABAL=1
URL_CABAL="https://hackage.haskell.org/package/${PACKAGE}/revision/${REVISION_CABAL}.cabal"

SHA256SUM="cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5"
SHA256SUM_CABAL="4b5a800fe9edf168fc7ae48c7a3fc2aab6b418ac15be2f1dad43c0f48a494a3b"

DEPENDS="zlib gmp ghc gcc"
