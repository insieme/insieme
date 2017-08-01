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
}

pkg_is_globally_installed() {
	local cur_ver="$(luajit -v | awk '{print $2}')"
	if [ -z "$cur_ver" ]; then
		return 1 # not installed
	fi

	local cmp_ver="$(printf "$VERSION\n$cur_ver" | sort -V | head -n1)"
	if [ "$cmp_ver" == "$cur_ver" ] && [ "$cur_ver" != "$VERSION" ]; then
		return 1 # not installed
	else
		return 0 # installed
	fi
}
