NAME="ghc"
VERSION="8.4.2"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE-x86_64-deb8-linux.tar.xz"

URL="https://downloads.haskell.org/~ghc/$VERSION/ghc-$VERSION-x86_64-deb8-linux.tar.xz"
SHA256SUM="246f66eb56f4ad0f1c7755502cfc8f9972f2d067dede17e151f6f479c1f76fbd"

DEPENDS="gmp"

GMP_PKG=$PREFIX/$(get_property gmp PACKAGE)

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp-includes="$GMP_PKG/include" \
		--with-gmp-libraries="$GMP_PKG/lib"
}

pkg_build() {
	true
}
