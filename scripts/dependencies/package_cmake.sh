NAME="cmake"
VERSION="3.2.1"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.gz"
URL="http://www.insieme-compiler.org/ext_libs/$FILE"
SHA256SUM="759f1cf6b1a26b037726a9acca6da501235c20ad3671df29d43f29052ef1502c"

pkg_configure() {
	LDFLAGS="-O3" ./configure --prefix="$PREFIX/$PACKAGE"
}
