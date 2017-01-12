NAME="cabal"
VERSION="1.24.0.0"
PACKAGE="$NAME-$VERSION"

FILE="cabal-install-$VERSION.tar.gz"
URL="https://www.haskell.org/cabal/release/cabal-install-$VERSION/$FILE"
SHA256SUM="d840ecfd0a95a96e956b57fb2f3e9c81d9fc160e1fd0ea350b0d37d169d9e87e"

DEPENDS="ghc gmp zlib"

GHC_PKG=$(get_property ghc PACKAGE)
GMP_PKG=$(get_property gmp PACKAGE)
ZLIB_PKG=$(get_property zlib PACKAGE)

export PATH="$PREFIX/$GHC_PKG/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/$GMP_PKG/lib:$PREFIX/$ZLIB_PKG/lib"

pkg_extract() {
	tar xf "$FILE"
	mv "cabal-install-$VERSION" "$PACKAGE"
}

pkg_configure() {
	mkdir -p "$PREFIX/$PACKAGE"
	ghc-pkg init "$PREFIX/$PACKAGE/packages.conf.d"
}

pkg_build() {
	true
}

pkg_install() {
	SCOPE_OF_INSTALLATION="--package-db=$PREFIX/$PACKAGE/packages.conf.d" \
	PREFIX="$PREFIX/$PACKAGE" ./bootstrap.sh --no-doc
}
