NAME="cmake"
VERSION="3.5.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="93d651a754bcf6f0124669646391dd5774c0fc4d407c384e3ae76ef9a60477e8"

pkg_configure() {
	LDFLAGS="-O3" ./configure --prefix="$PREFIX/$PACKAGE"
}

pkg_is_globally_installed() {
	local cur_ver="$(/usr/bin/cmake --version 2>/dev/null | head -n1 | grep -oE '[^ ]+$')"
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
