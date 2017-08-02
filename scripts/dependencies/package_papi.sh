NAME="papi"
VERSION="5.4.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="537bc209ad05050399d5f268ba8f40e499f457cab9b3503f57600f9893819195"

pkg_configure() {
	cd src
	./configure --prefix="$PREFIX/$PACKAGE" --with-tests=ctests
}

pkg_install() {
	make install PREFIX="$PREFIX/$PACKAGE"
}

pkg_is_globally_installed() {
	local cur_ver="$(papi_version | grep -oE '[^ ]+$')"
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
