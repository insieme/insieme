NAME="boost"
VERSION="1.50.0"
PACKAGE="$NAME-$VERSION"

FILE="${NAME}_${VERSION//./_}.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="c9ace2b8c81fa6703d1d17c7e478de3bc51101c5adbdeb3f6cb72cf3045a8529"

BOOST_LIBS="filesystem,program_options,random,system,regex,thread,serialization,date_time,wave"

pkg_extract() {
	tar xf "$FILE"
	mv "${NAME}_${VERSION//./_}" "$PACKAGE"
}

pkg_configure() {
	./bootstrap.sh --prefix="$PREFIX/$PACKAGE" --with-libraries="$BOOST_LIBS"
}

pkg_build() {
	true
}

pkg_install() {
	./b2 cxxflags="$CFLAGS" release install "-j$SLOTS"
	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
}
