NAME="stack"
VERSION="1.0.4"
PACKAGE="$NAME-$VERSION"

FILE="v$VERSION.tar.gz"
URL="https://github.com/commercialhaskell/stack/archive/$FILE"
SHA256SUM="60df5eaeccd9db7fdb535f056815c9ec196731231d4754d2e294f74bef3f4547"

FILE_PB="$PACKAGE-linux-x86_64.tar.gz"
URL_PB="https://github.com/commercialhaskell/stack/releases/download/v$VERSION/$FILE_PB"
SHA256SUM_PB="fedf161622170801f29be5d5096ea30e253b2bba54f185607f568c44ee151e5a"

DEPENDS="gcc gmp zlib"

export CPATH="$PREFIX/gmp-latest/include:$PREFIX/zlib-latest/include"
export LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib"
export LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib"
export STACK_ROOT="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")/$PACKAGE-linux-x86_64/stack-root"

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
	"../$PACKAGE-linux-x86_64/stack" setup
}

pkg_build() {
	"../$PACKAGE-linux-x86_64/stack" build
}

pkg_install() {
	mkdir -p "$PREFIX/$PACKAGE/bin"
	cp "$(find .stack-work/install -name stack)" "$PREFIX/$PACKAGE/bin/stack"

	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$PACKAGE-linux-x86_64" "$FILE" "$FILE_PB"
}
