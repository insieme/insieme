NAME="ghc"
VERSION="7.10.3"
PACKAGE="$NAME-$VERSION"

FILE="ghc-7.10.3b-x86_64-deb8-linux.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="804c75c4635353bf987c1ca120b8531c7bb4957c5b84d29c7adc4894b6fd579d"
# signed by Benjamin Gamari <ben@well-typed.com>
# FFEB 7CE8 1E16 A36B 3E2D  ED6F 2DE0 4D4E 97DB 64AD

DEPENDS="gmp"

export LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp-includes="$PREFIX/gmp-latest/include" \
		--with-gmp-libraries="$PREFIX/gmp-latest/lib"
}

pkg_build() {
	true
}
