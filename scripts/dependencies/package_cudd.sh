NAME="cudd"
VERSION="2.4.2"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="9dd7c9926258e2557bbd61328d0e685b7ac508af300cc54175b5dec8138a7175"

pkg_configure() {
	true
}

pkg_build() {
	make -j "$SLOTS" XCFLAGS="-DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC"
}

pkg_check() {
	make -j "$SLOTS" XCFLAGS="-DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" testobj
}

pkg_install() {
	mkdir lib
	rm -f ./obj/testobj.o
	rm -rf nanotrav

	# merge all libraries
	find -name "*.o" -exec ar -r ./lib/libcudd.a {} \;

	# move result to prefix
	cd ..
	mv "$PACKAGE" "$PREFIX/$PACKAGE"
}
