NAME="gcc"
VERSION="5.1.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="335275817b5ed845fee787e75efd76a6e240bfabbe0a0c20a81a04777e204617"

DEPENDS="gmp mpc mpfr"

GMP_PKG=$(get_property gmp PACKAGE)
MPC_PKG=$(get_property mpc PACKAGE)
MPFR_PKG=$(get_property mpfr PACKAGE)

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$PREFIX/$GMP_PKG/lib:$PREFIX/$MPC_PKG/lib:$PREFIX/$MPFR_PKG/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp="$PREFIX/$GMP_PKG" \
		--with-mpc="$PREFIX/$MPC_PKG" \
		--with-mpfr="$PREFIX/$MPFR_PKG" \
		--enable-languages="c,c++" \
		--without-isl \
		--disable-multilib \
		--enable-lto
}
