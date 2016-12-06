NAME="python"
VERSION="2.7.12"
PACKAGE="$NAME-$VERSION"

FILE="Python-$VERSION.tgz"
URL="https://www.python.org/ftp/python/2.7.12/Python-2.7.12.tgz"
SHA256SUM="3cb522d17463dfa69a155ab18cffa399b358c966c0363d6c8b5b3bf1384da4b6"

pkg_extract() {
	tar xf "$FILE"
	mv "Python-$VERSION" "$PACKAGE"
}
