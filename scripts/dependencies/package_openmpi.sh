NAME="openmpi"
VERSION="3.0.1"
PACKAGE="$NAME-$VERSION"

FILE="openmpi-$VERSION.tar.bz2"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="663450d1ee7838b03644507e8a76edfb1fba23e601e9e0b5b2a738e54acd785d"

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" --disable-mpi-fortran
}
