NAME="hwloc"
VERSION="1.10.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="ef7cc854a26a9bdef2bc5f14c8d627b7c4a34e3a2fd06aeb3dec2b3b0cc364fc"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --disable-libxml2
}

pkg_check() {
	make check
}

pkg_is_globally_installed() {
	local cur_ver="$(/usr/bin/hwloc-info --version 2>/dev/null | grep -oE '[^ ]+$')"
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
