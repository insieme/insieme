NAME="ghc"
VERSION="7.10.3"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE-x86_64-centos67-linux.tar.bz2"
URL="http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-centos67-linux.tar.bz2"
SHA256SUM="a8957f7a2fd81720c5d3dc403571d77d31115ff5f42edb2917c36d8e714220d4"

DEPENDS="gmp"

GMP_PKG=$(get_property gmp PACKAGE)

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp-includes="$PREFIX/$GMP_PKG/include" \
		--with-gmp-libraries="$PREFIX/$GMP_PKG/lib"
}

pkg_build() {
	true
}
