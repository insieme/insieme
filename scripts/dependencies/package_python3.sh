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
