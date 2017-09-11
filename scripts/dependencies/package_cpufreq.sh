NAME="cpufrequtils"
VERSION="008"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="a2149db551f83112209b1a8e79bd50d386979bbf64edbc69126f4e0b4f0a4cab"

pkg_configure() {
	true
}

pkg_install() {
	DESTDIR="$PREFIX/$PACKAGE" NLS=false bindir=/bin includedir=/include libdir=/lib mandir=/man make install
}
