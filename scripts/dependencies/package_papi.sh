NAME="papi"
VERSION="5.4.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="537bc209ad05050399d5f268ba8f40e499f457cab9b3503f57600f9893819195"

pkg_configure() {
	cd src
	CFLAGS="$CFLAGS -Wno-format-truncation" ./configure --prefix="$PREFIX/$PACKAGE" --with-tests=ctests
}

pkg_install() {
	make install PREFIX="$PREFIX/$PACKAGE"
}
