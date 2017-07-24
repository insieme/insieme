NAME="gcc"
VERSION="6.3.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="02f9302a559fa2251595ca0bc1e937219eff2995a3802d7b31676fec2402beb4"

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

pkg_is_globally_installed() {
	local currentver
	currentver="$(gcc -dumpversion)"
	if [ "$(printf "$VERSION\n$currentver" | sort -V | head -n1)" == "$currentver" ] && [ "$currentver" != "$VERSION" ]
	then
        return 1 # not installed
	else
		return 0 # is installed
	fi
}
