NAME="binutils"
VERSION="2.27"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="26253bf0f360ceeba1d9ab6965c57c6a48a01a8343382130d1ed47c468a3094f"

DEPENDS="bison"

export PATH="$(get_pkg_prefix bison)/bin:$PATH"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --enable-gold
}
