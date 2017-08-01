NAME="libtool"
VERSION="2.4.5"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="84aac136513b009278896ffa255e4d685bcdb0cb0e5363be36adad64c986177e"

DEPENDS="autoconf automake"

export PATH="$PREFIX/$(get_property autoconf PACKAGE)/bin:$PATH"
export PATH="$PREFIX/$(get_property automake PACKAGE)/bin:$PATH"

pkg_is_globally_installed() {
	local cur_ver="$(libtool --version | head -n1 | grep -oE '[^ ]+$')"
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
