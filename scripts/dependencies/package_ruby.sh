NAME="ruby"
VERSION="1.9.3-p125"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="8b3c035cf4f0ad6420f447d6a48e8817e5384d0504514939aeb156e251d44cce"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --enable-shared --disable-install-doc
}

pkg_is_globally_installed() {
	local cur_ver="$(ruby -e 'puts RUBY_VERSION')"
	if [ -z "$cur_ver" ]; then
		return 1 # not installed
	fi

	local cmp_ver="$(printf "$VERSION\n$cur_ver" | sort -V | head -n1)"
	if [ "$cmp_ver" == "$cur_ver" ] && [ "$cur_ver" != "$VERSION" ]; then
		return 1 # not installed
	else
		return 0 # is installed
	fi
}
