NAME="mpc"
VERSION="0.9"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="fd3efe422f0d454592059e80f2c00d1a2e381bf2beda424c5094abd4deb049ac"

DEPENDS="gmp mpfr"

GMP_PKG=$(get_property gmp PACKAGE)
MPFR_PKG=$(get_property mpfr PACKAGE)

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$PREFIX/$GMP_PKG/lib:$PREFIX/$MPFR_PKG/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp="$PREFIX/$GMP_PKG" \
		--with-mpfr="$PREFIX/$MPFR_PKG"
}

pkg_check() {
	make check
}
