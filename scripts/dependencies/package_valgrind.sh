NAME="valgrind"
VERSION="3.11.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="6c396271a8c1ddd5a6fb9abe714ea1e8a86fce85b30ab26b4266aeb4c2413b42"

export CFLAGS="-O3 -fPIC"
export LDFLAGS="-O3 -fPIC"

pkg_is_globally_installed() {
	local cur_ver="$(valgrind --version | grep -oE '[^-]+$')"
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
