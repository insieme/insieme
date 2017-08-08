NAME="automake"
VERSION="1.15"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="9908c75aabd49d13661d6dcb1bc382252d22cc77bf733a2d55e87f2aa2db8636"

DEPENDS="autoconf"

export PATH="$(get_pkg_prefix autoconf)/bin:$PATH"

pkg_is_globally_installed() {
	local cur_ver="$(/usr/bin/automake --version 2>/dev/null | head -n1 | grep -oE '[^ ]+$')"
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
