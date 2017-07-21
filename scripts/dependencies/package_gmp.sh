NAME="gmp"
VERSION="6.0.0"
SUBVERSION="a"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE$SUBVERSION.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="7f8e9a804b9c6d07164cf754207be838ece1219425d64e28cfa3e70d5c759aaf"

unset CC CXX LD_LIBRARY_PATH

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --enable-cxx
}

pkg_check() {
	make check
}

pkg_is_globally_installed() {
	cat > libgmp10-test.c <<EOF
int main() { __gmpf_cmp_z(); }
EOF
	cc -o /dev/null libgmp10-test.c -lgmp  2>/dev/null
	RV=$?
	return $RV
}
