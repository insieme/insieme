NAME="Cabal"
VERSION="cd120c2578973f5ce0bfe7341ddf7250d533a15c"
PACKAGE="$NAME-$VERSION"

FILE="${PACKAGE}.tar.gz"
URL="https://github.com/haskell/cabal.git"

DEPENDS="zlib gmp ghc gcc  haskell/parsec haskell/mtl haskell/text"

pkg_download() {
	git clone "$URL" "cabal.git"
	( cd "cabal.git" && git checkout "$VERSION" )
	ln -s cabal.git/Cabal "$PACKAGE"
}

pkg_extract() {
    true
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "${PACKAGE}.cabal" "$FILE" cabal.git/
}
