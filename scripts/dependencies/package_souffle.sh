NAME="souffle"
VERSION="2016.11.18"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.zip"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="ffc5a36b66b1298a66631e0f89f47ff83a0af7afb8872a24346158e8d50b1cd4"

DEPENDS="autoconf automake libtool bison flex boost sqlite"

AUTOCONF_PKG=$PREFIX/$(get_property autoconf PACKAGE)
AUTOMAKE_PKG=$PREFIX/$(get_property automake PACKAGE)
LIBTOOL_PKG=$PREFIX/$(get_property libtool PACKAGE)
BISON_PKG=$PREFIX/$(get_property bison PACKAGE)
FLEX_PKG=$PREFIX/$(get_property flex PACKAGE)
BOOST_PKG=$PREFIX/$(get_property boost PACKAGE)
SQLITE_PKG=$PREFIX/$(get_property sqlite PACKAGE)

export PATH="$AUTOCONF_PKG/bin:$AUTOMAKE_PKG/bin:$PATH"
export PATH="$LIBTOOL_PKG/bin:$PATH"
export PATH="$BISON_PKG/bin:$FLEX_PKG/bin:$PATH"

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
	pwd
	sh ./bootstrap
	./configure \
		--prefix="$PREFIX/$PACKAGE" \
		--with-boost="$BOOST_PKG" \
		--disable-java \
		CPPFLAGS="-I$SQLITE_PKG/include" \
		LDFLAGS="-L$SQLITE_PKG/lib -Wl,-rpath,$BISON_PKG/lib -Wl,-rpath,$BOOST_PKG/lib"
}

pkg_build() {
	make -j "$SLOTS" || true
	make -j "$SLOTS"
}
