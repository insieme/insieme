NAME="stack"
VERSION="1.4.0"
PACKAGE="$NAME-$VERSION"

FILE="v$VERSION.tar.gz"
URL="https://github.com/commercialhaskell/stack/archive/$FILE"
SHA256SUM="595d311ad117e41ad908b7065743917542b40f343d1334673e98171ee74d36e6"

FILE_PB="$PACKAGE-linux-x86_64.tar.gz"
URL_PB="https://github.com/commercialhaskell/stack/releases/download/v$VERSION/$FILE_PB"
SHA256SUM_PB="618a309d763432a2cca654bc29249a77c7de096c693a28b84dd3656470269f5a"

DEPENDS="gmp zlib"

GMP_PKG=$(get_property gmp PACKAGE)
ZLIB_PKG=$(get_property zlib PACKAGE)

export CPATH="$PREFIX/$GMP_PKG/include:$PREFIX/$ZLIB_PKG/include"
export LIBRARY_PATH="$PREFIX/$GMP_PKG/lib:$PREFIX/$ZLIB_PKG/lib"
export LD_LIBRARY_PATH="$PREFIX/$GMP_PKG/lib:$PREFIX/$ZLIB_PKG/lib"

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
