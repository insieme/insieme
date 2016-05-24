NAME="boost"
VERSION="1.58.0"
PACKAGE="$NAME-$VERSION"

FILE="${NAME}_${VERSION//./_}.tar.bz2"
URL="http://downloads.sourceforge.net/project/boost/boost/$VERSION/$FILE"
SHA256SUM="fdfc204fc33ec79c99b9a74944c3e54bd78be4f7f15e260c0e2700a36dc7d3e5"

DEPENDS="gcc"

export PATH="$PREFIX/gcc-latest/bin:$PATH"

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
