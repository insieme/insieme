NAME="gcc"
VERSION="6.3.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="02f9302a559fa2251595ca0bc1e937219eff2995a3802d7b31676fec2402beb4"

DEPENDS="gmp mpc mpfr"

GMP_PKG=$(get_pkg_prefix gmp)
MPC_PKG=$(get_pkg_prefix mpc)
MPFR_PKG=$(get_pkg_prefix mpfr)

unset CC CXX LD_LIBRARY_PATH
export LD_RUN_PATH="$GMP_PKG/lib:$MPC_PKG/lib:$MPFR_PKG/lib"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp="$GMP_PKG" \
		--with-mpc="$MPC_PKG" \
		--with-mpfr="$MPFR_PKG" \
		--enable-languages="c,c++" \
		--without-isl \
		--disable-multilib \
		--enable-lto
}

pkg_is_globally_installed() {
	local cur_ver="$(gcc -dumpversion)"
	if [ -z "$cur_ver" ]; then
		return 1 # not installed
	fi

	local cmp_ver="$(printf "$VERSION\n$cur_ver" | sort -V | head -n1)"
	if [ "$cmp_ver" == "$cur_ver" ] && [ "$cur_ver" != "$VERSION" ]; then
		return 1 # not installed
	else
		return 0 # is installed
	fi
}
