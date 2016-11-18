NAME="automake"
VERSION="1.15"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="9908c75aabd49d13661d6dcb1bc382252d22cc77bf733a2d55e87f2aa2db8636"

DEPENDS="autoconf"

export PATH="$PREFIX/autoconf-latest/bin:$PATH"
