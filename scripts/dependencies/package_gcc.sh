NAME="gcc"
VERSION="5.4.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="37089e80c3f2e9a0663d7ccc51c2a6c7dbbf3275bc1e4ed1ed3b1460cd5b3030"

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
