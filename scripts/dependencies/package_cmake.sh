NAME="cmake"
VERSION="3.5.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="93d651a754bcf6f0124669646391dd5774c0fc4d407c384e3ae76ef9a60477e8"

pkg_configure() {
	LDFLAGS="-O3" ./configure --prefix="$PREFIX/$PACKAGE"
}
