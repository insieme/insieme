NAME="gcc"
VERSION="5.1.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="335275817b5ed845fee787e75efd76a6e240bfabbe0a0c20a81a04777e204617"

DEPENDS="gmp mpc mpfr"

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp="$PREFIX/gmp-latest" \
		--with-mpc="$PREFIX/mpc-latest" \
		--with-mpfr="$PREFIX/mpfr-latest" \
		--enable-languages="c,c++" \
		--without-isl \
		--disable-multilib \
		--enable-lto
}
