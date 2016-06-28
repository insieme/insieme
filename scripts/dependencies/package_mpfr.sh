NAME="mpfr"
VERSION="3.1.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="7b66c3f13dc8385f08264c805853f3e1a8eedab8071d582f3e661971c9acd5fd"

DEPENDS="gmp"

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$PREFIX/gmp-latest/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --with-gmp="$PREFIX/gmp-latest"
}

pkg_check() {
	make check
}
