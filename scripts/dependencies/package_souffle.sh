NAME="souffle"
VERSION="2016.05.15"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.zip"
URL="http://www.dps.uibk.ac.at/~csaf7445/ext_libs/$FILE"
SHA256SUM="35d4f7d8db604dfa1ba785fce3cefd439838f290d2618281a85ba79819ec19bc"

DEPENDS="gcc autoconf automake bison flex libtool boost"

export PATH="$PREFIX/autoconf-latest/bin:$PREFIX/automake-latest/bin:$PATH"
export PATH="$PREFIX/bison-latest/bin:$PREFIX/flex-latest/bin:$PATH"
export PATH="$PREFIX/libtool-latest/bin:$PATH"

export BOOST_ROOT="$PREFIX/boost-latest"

pkg_extract() {
	unzip -o -d "$PACKAGE" "$FILE"
	mv "$PACKAGE/souffle/"* "$PACKAGE"
}

pkg_prepare() {
	find "../patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -N -i
	cp "../patches/boost.m4" "m4/"
}

pkg_configure() {
	libtoolize
	aclocal
	autoheader
	automake --gnu --add-missing
	autoconf || autoconf # Fails the first time
	./configure --prefix="$PREFIX/$PACKAGE"
}
