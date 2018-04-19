NAME="moreutils"
VERSION="0.60"
PACKAGE="$NAME-$VERSION"

FILE="${NAME}_${VERSION}.orig.tar.xz"
URL="http://http.debian.net/debian/pool/main/m/moreutils/moreutils_${VERSION}.orig.tar.xz"
SHA256SUM="e42d18bacbd2d003779a55fb3542befa5d1d217ee37c1874e8c497581ebc17c5"

DEPENDS=""

pkg_configure() {
    : # pacakge doesn't have a configure script, just a makefile
}

pkg_build() {
    MANS='' make -e all
}

pkg_install() {
    MANS='/dev/null' make -e install PREFIX="$PREFIX/$PACKAGE"
}
