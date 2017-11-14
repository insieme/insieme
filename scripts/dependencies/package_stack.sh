NAME="stack"
VERSION="1.0.4"
PACKAGE="$NAME-$VERSION"

FILE="v$VERSION.tar.gz"
URL="https://github.com/commercialhaskell/stack/archive/$FILE"
SHA256SUM="60df5eaeccd9db7fdb535f056815c9ec196731231d4754d2e294f74bef3f4547"

FILE_PB="$PACKAGE-linux-x86_64.tar.gz"
URL_PB="https://github.com/commercialhaskell/stack/releases/download/v$VERSION/$FILE_PB"
SHA256SUM_PB="fedf161622170801f29be5d5096ea30e253b2bba54f185607f568c44ee151e5a"

DEPENDS="gmp zlib"

GMP_PKG=$PREFIX/$(get_property gmp PACKAGE)
ZLIB_PKG=$PREFIX/$(get_property zlib PACKAGE)

export CPATH="$GMP_PKG/include:$ZLIB_PKG/include"
export LIBRARY_PATH="$GMP_PKG/lib:$ZLIB_PKG/lib"
export LD_LIBRARY_PATH="$GMP_PKG/lib:$ZLIB_PKG/lib"

pkg_download() {
	wget -nc "$URL"
	echo "$SHA256SUM  $FILE" | sha256sum -c
	wget -nc "$URL_PB"
	echo "$SHA256SUM_PB  $FILE_PB" | sha256sum -c
}

pkg_extract() {
	tar xf "$FILE"
	tar xf "$FILE_PB"
}

pkg_configure() {
	STACK_ROOT="$PKG_TEMP/stack-root" "../$PACKAGE-linux-x86_64/stack" setup
}

pkg_build() {
	STACK_ROOT="$PKG_TEMP/stack-root" "../$PACKAGE-linux-x86_64/stack" build
}

pkg_install() {
	mkdir -p "$PREFIX/$PACKAGE/bin"
	cp "$(find .stack-work/install -name stack)" "$PREFIX/$PACKAGE/bin/stack"

	chmod g+w "$PREFIX/$PACKAGE"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$PACKAGE-linux-x86_64" "stack-root" "$FILE" "$FILE_PB"
}
