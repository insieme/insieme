NAME="hwloc"
VERSION="1.10.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="ef7cc854a26a9bdef2bc5f14c8d627b7c4a34e3a2fd06aeb3dec2b3b0cc364fc"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --disable-libxml2
}

pkg_check() {
	make check
}
