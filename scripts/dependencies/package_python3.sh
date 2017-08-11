NAME="python3"
VERSION="3.5.2"
PACKAGE="$NAME-$VERSION"

FILE="Python-$VERSION.tgz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="1524b840e42cf3b909e8f8df67c1724012c7dc7f9d076d4feef2d3eff031e8a0"

pkg_extract() {
	tar xf "$FILE"
	mv "Python-$VERSION" "$PACKAGE"
}

pkg_is_globally_installed() {
	local cur_ver="$(/usr/bin/python3 -c 'import platform; print(platform.python_version())' 2>/dev/null)"
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
