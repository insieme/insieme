NAME="zlib"
VERSION="1.2.8"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="831df043236df8e9a7667b9e3bb37e1fcb1220a0f163b6de2626774b9590d057"

pkg_is_globally_installed() {
	pkg-config zlib --atleast-version=1.2
}
