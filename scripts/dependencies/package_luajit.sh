NAME="luajit"
VERSION="2.0.3"
PACKAGE="$NAME-$VERSION"

FILE="LuaJIT-$VERSION.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="55be6cb2d101ed38acca32c5b1f99ae345904b365b642203194c585d27bebd79"

pkg_extract() {
	tar xf "$FILE"
	mv "LuaJIT-$VERSION" "$PACKAGE"
}

pkg_configure() {
	true
}

pkg_build() {
	make -j "$SLOTS" CFLAGS="-O3 -fPIC" LDFLAGS="-O3 -fPIC"
}

pkg_install() {
	make install PREFIX="$PREFIX/$PACKAGE"
	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
	touch "$PREFIX/$PACKAGE/.installed"
}
