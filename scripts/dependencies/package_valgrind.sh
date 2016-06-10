NAME="valgrind"
VERSION="3.11.0"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="6c396271a8c1ddd5a6fb9abe714ea1e8a86fce85b30ab26b4266aeb4c2413b42"

export CFLAGS="-O3 -fPIC"
export LDFLAGS="-O3 -fPIC"
