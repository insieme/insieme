NAME="libtool"
VERSION="2.4.5"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"
URL="http://ftp.ntua.gr/mirror/gnu/libtool/$FILE"
SHA256SUM="84aac136513b009278896ffa255e4d685bcdb0cb0e5363be36adad64c986177e"

DEPENDS="autoconf automake"

export PATH="$PREFIX/autoconf-latest/bin:$PREFIX/automake-latest/bin:$PATH"
