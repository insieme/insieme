NAME="autoconf"
VERSION="2.68"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="c994778716e6223cf65e898e06c15ed498fe81424838adf67007282b661055ba"

pkg_is_globally_installed() {
	local cur_ver="$(/usr/bin/autoconf --version 2>/dev/null | head -n1 | grep -oE '[^ ]+$')"
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
