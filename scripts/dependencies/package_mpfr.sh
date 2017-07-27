NAME="mpfr"
VERSION="3.1.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="7b66c3f13dc8385f08264c805853f3e1a8eedab8071d582f3e661971c9acd5fd"

DEPENDS="gmp"

GMP_PKG=$(get_property gmp PACKAGE)

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$PREFIX/$GMP_PKG/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --with-gmp="$PREFIX/$GMP_PKG"
}

pkg_check() {
	make check
}

pkg_is_globally_installed() {
	cat > mpfr-test.c <<EOF
#include <mpfr.h>
#if MPFR_VERSION < MPFR_VERSION_NUM($(printf "$VERSION" | tr . ,))
choke me
#endif
int main() { return 0; }
EOF
	cc -o /dev/null mpfr-test.c 2>/dev/null
	RV=$?
        rm mpfr-test.c
	return $RV
}
