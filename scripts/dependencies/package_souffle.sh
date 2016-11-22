NAME="souffle"
VERSION="2016.11.18"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.zip"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="ffc5a36b66b1298a66631e0f89f47ff83a0af7afb8872a24346158e8d50b1cd4"

DEPENDS="bison flex boost sqlite"

export PATH="$PREFIX/autoconf-latest/bin:$PREFIX/automake-latest/bin:$PATH"
export PATH="$PREFIX/libtool-latest/bin:$PATH"
export PATH="$PREFIX/bison-latest/bin:$PREFIX/flex-latest/bin:$PATH"

export BOOST_ROOT="$PREFIX/boost-latest"

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
		--with-boost="$BOOST_ROOT" \
		--disable-java \
		CPPFLAGS="-I$PREFIX/sqlite-latest/include" \
		LDFLAGS="-L$PREFIX/sqlite-latest/lib -Wl,-rpath,$BOOST_ROOT/lib"
}
