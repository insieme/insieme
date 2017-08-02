NAME="python"
VERSION="2.7.12"
PACKAGE="$NAME-$VERSION"

FILE="Python-$VERSION.tgz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="3cb522d17463dfa69a155ab18cffa399b358c966c0363d6c8b5b3bf1384da4b6"

pkg_extract() {
	tar xf "$FILE"
	mv "Python-$VERSION" "$PACKAGE"
}

pkg_is_globally_installed() {
	local cur_ver="$(python2 -c 'import platform; print(platform.python_version())')"
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
