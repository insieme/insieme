NAME="boost"
VERSION="1.59.0"
PACKAGE="$NAME-$VERSION"

FILE="${NAME}_${VERSION//./_}.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="727a932322d94287b62abb1bd2d41723eec4356a7728909e38adb65ca25241ca"

BOOST_LIBS="date_time,filesystem,program_options,regex,system,serialization,thread,wave"

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
}
