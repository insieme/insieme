NAME="cabal"
VERSION="b2f8cb895180da84611874c445863cb49a30bc63"
PACKAGE="$NAME-$VERSION"

URL="https://github.com/DanielG/cabal.git"

DEPENDS="zlib gmp ghc gcc"

pkg_download() {
	git clone "$URL" "$PACKAGE"
	( cd "$PACKAGE" && git checkout "$VERSION" )
}

pkg_extract() {
	true
}

pkg_configure() {
    pkg_haskell_setup_env
}

pkg_build() {
	true
}

pkg_install() {
	cd cabal-install/

        PACKAGE_DB="$PREFIX/$PACKAGE"/package.conf.d
        [[ ! -f "${PACKAGE_DB}/package.cache" ]] && ghc-pkg init "$PACKAGE_DB"

	env \
		EXTRA_CONFIGURE_OPTS="--enable-shared --disable-executable-dynamic" \
		SCOPE_OF_INSTALLATION="--package-db=$PACKAGE_DB" \
		PREFIX="$PREFIX/$PACKAGE" \
		sh -x ./bootstrap.sh --no-doc -j

	[ -x "$PREFIX/$PACKAGE/bin/cabal" ]
}
