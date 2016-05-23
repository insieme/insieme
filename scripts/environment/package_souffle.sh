NAME="souffle"
VERSION="2016.05.15"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.zip"
URL="http://www.dps.uibk.ac.at/~csaf7445/ext_libs/$FILE"
SHA256SUM="35d4f7d8db604dfa1ba785fce3cefd439838f290d2618281a85ba79819ec19bc"

DEPENDS="gcc autoconf automake bison flex libtool"

export PATH="$PREFIX/autoconf-latest/bin:$PREFIX/automake-latest/bin:$PATH"
export PATH="$PREFIX/bison-latest/bin:$PREFIX/flex-latest/bin:$PATH"
export PATH="$PREFIX/libtool-latest/bin:$PATH"

echo "Script has been disabled for now"; false

pkg_extract() {
	unzip "$FILE"
	mv "$NAME" "$PACKAGE"
}

pkg_prepare() {
	find "../patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -i
}

pkg_configure() {
	sh bootstrap
	./configure --prefix="$PREFIX/$PACKAGE"
}
