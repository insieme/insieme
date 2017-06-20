NAME="souffle"
VERSION="2016.11.18"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.zip"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="ffc5a36b66b1298a66631e0f89f47ff83a0af7afb8872a24346158e8d50b1cd4"

DEPENDS="autoconf automake libtool bison flex boost sqlite"

AUTOCONF_PKG=$(get_property autoconf PACKAGE)
AUTOMAKE_PKG=$(get_property automake PACKAGE)
LIBTOOL_PKG=$(get_property libtool PACKAGE)
BISON_PKG=$(get_property bison PACKAGE)
FLEX_PKG=$(get_property flex PACKAGE)
BOOST_PKG=$(get_property boost PACKAGE)
SQLITE_PKG=$(get_property sqlite PACKAGE)

export PATH="$PREFIX/$AUTOCONF_PKG/bin:$PREFIX/$AUTOMAKE_PKG/bin:$PATH"
export PATH="$PREFIX/$LIBTOOL_PKG/bin:$PATH"
export PATH="$PREFIX/$BISON_PKG/bin:$PREFIX/$FLEX_PKG/bin:$PATH"

pkg_extract() {
	unzip -o -d "$PACKAGE" "$FILE"
	mv "$PACKAGE/souffle-master/"* "$PACKAGE"
}

pkg_prepare() {
	patch -N <<-EOF
	--- configure.ac
	+++ configure.ac
	@@ -7,1 +7,1 @@
	-AC_INIT(souffle, m4_esyscmd([git describe --tags --abbrev=0 --always | tr -d '\n']), [souffle-talk@googlegroups.com])
	+AC_INIT(souffle, $PACKAGE, [souffle-talk@googlegroups.com])
	EOF

	find "$INSTALLER_DIR/patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -N -i
}

pkg_configure() {
	# TODO link souffle binaries with rpath against boost

	pwd
	sh ./bootstrap
	./configure \
		--prefix="$PREFIX/$PACKAGE" \
		--with-boost="$PREFIX/$BOOST_PKG" \
		--disable-java \
		CPPFLAGS="-I$PREFIX/$SQLITE_PKG/include" \
		LDFLAGS="-L$PREFIX/$SQLITE_PKG/lib -Wl,-rpath,$PREFIX/$BISON_PKG/lib"
}
