NAME="cmake"
VERSION="3.7.2"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="dc1246c4e6d168ea4d6e042cfba577c1acd65feea27e56f5ff37df920c30cae0"

pkg_configure() {
	LDFLAGS="-O3" ./configure --prefix="$PREFIX/$PACKAGE"
}
