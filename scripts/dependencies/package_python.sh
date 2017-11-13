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
