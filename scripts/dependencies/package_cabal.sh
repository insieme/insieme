NAME="cabal"
VERSION="453aa45a3467662a5815f5d4b5ee345d04907fbc"
PACKAGE="$NAME-$VERSION"

URL="https://github.com/haskell/cabal.git"

DEPENDS="zlib gmp gcc ghc"

export PATH="$PREFIX/ghc-latest/bin:$PREFIX/gcc-latest/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib"
export LIBRARY_PATH="$PREFIX/gmp-latest/lib:$PREFIX/zlib-latest/lib"
export C_INCLUDE_PATH="$PREFIX/zlib-latest/include"


pkg_download() {
	git clone --single-branch "$URL" "$PACKAGE"
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
	ghc -j --make Setup.hs
	./Setup configure \
			--package-db="$PKG_TEMP"/packages.conf.d \
			--prefix="$PREFIX/$PACKAGE" \
			--enable-shared
	./Setup build -j
	./Setup install
	popd

	pushd cabal-install/
	env \
		EXTRA_CONFIGURE_OPTS="--enable-shared --disable-executable-dynamic" \
		SCOPE_OF_INSTALLATION=--package-db="$PKGS" \
		PREFIX="$PREFIX/$PACKAGE" \
		sh -x ./bootstrap.sh --no-doc
	popd

	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
	touch "$PREFIX/$PACKAGE/.installed"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$FILE"
	rm -rf "$PKGS"
}
