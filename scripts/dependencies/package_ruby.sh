NAME="ruby"
VERSION="1.9.3-p125"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="8b3c035cf4f0ad6420f447d6a48e8817e5384d0504514939aeb156e251d44cce"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --enable-shared --disable-install-doc
}
