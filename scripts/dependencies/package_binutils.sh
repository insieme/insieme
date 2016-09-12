NAME="binutils"
VERSION="2.27"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="https://ftp.gnu.org/gnu/binutils/$FILE"
SHA256SUM="26253bf0f360ceeba1d9ab6965c57c6a48a01a8343382130d1ed47c468a3094f"

DEPENDS="bison"

export PATH="$PREFIX/bison-latest/bin:$PATH"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --enable-gold
}
