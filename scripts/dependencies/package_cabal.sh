NAME="cabal"
VERSION="987570d8f2c85d28bd1c75fe9a37de7291b1b180"
PACKAGE="$NAME-$VERSION"

URL="https://github.com/haskell/cabal.git"

DEPENDS="zlib gmp ghc"

GCC_PKG=$PREFIX/$(get_property gcc PACKAGE)
GHC_PKG=$PREFIX/$(get_property ghc PACKAGE)
ZLIB_PKG=$PREFIX/$(get_property zlib PACKAGE)
GMP_PKG=$PREFIX/$(get_property gmp PACKAGE)

export PATH="$GHC_PKG/bin:$GCC_PKG/bin:$PATH"
export LD_LIBRARY_PATH="$GMP_PKG/lib:$ZLIB_PKG/lib"
export LIBRARY_PATH="$GMP_PKG/lib:$ZLIB_PKG/lib"
export C_INCLUDE_PATH="$ZLIB_PKG/include"


pkg_download() {
	git clone "$URL" "$PACKAGE"
	( cd "$PACKAGE"; git checkout "$VERSION" )
}

pkg_extract() {
	true
}

pkg_configure() {
	true
}

pkg_build() {
	true
}

pkg_install() {
	PKGS="$PKG_TEMP"/packages.conf.d; ghc-pkg init "$PKGS"

	pushd Cabal/
	ghc -j8 --make Setup.hs
	./Setup configure \
			--package-db="$PKG_TEMP"/packages.conf.d \
			--prefix="$PREFIX/$PACKAGE" \
			--enable-shared
	./Setup build -j8
	./Setup install
	popd

	pushd cabal-install/
	env \
		EXTRA_CONFIGURE_OPTS="--enable-shared --disable-executable-dynamic" \
		SCOPE_OF_INSTALLATION=--package-db="$PKGS" \
		PREFIX="$PREFIX/$PACKAGE" \
		sh -x ./bootstrap.sh --no-doc
	popd

	[ ! -x "$PREFIX/$PACKAGE/bin/cabal" ] && exit 1

	touch "$PREFIX/$PACKAGE/.installed"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$FILE"
	rm -rf "$PKGS"
}
