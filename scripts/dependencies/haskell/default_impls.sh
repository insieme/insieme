# This file contains the default implementation for install step.s

pkg_download() {
	wget -nc "$URL"
	wget -nc "$URL_CABAL" -O $PACKAGE.cabal
	if [[ "$SHA256SUM" ]]; then
		echo "$SHA256SUM  $FILE" | sha256sum -c
        else
            sha256sum "$FILE"
	fi

	if [[ "$SHA256SUM_CABAL" ]]; then
		echo "$SHA256SUM_CABAL  ${PACKAGE}.cabal" | sha256sum -c
        else
            sha256sum "${PACKAGE}.cabal"
	fi

        if [[ ! "$SHA256SUM_CABAL" || ! "$SHA256SUM" ]]; then
            exit 1
        fi

}

pkg_extract() {
	tar xf "$FILE"
        [ -f "${PACKAGE}.cabal" ] && cp "${PACKAGE}.cabal" "$PACKAGE"/"${NAME}".cabal
}

pkg_prepare() {
	find "$INSTALLER_DIR/patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -N -i
}

declare -a PACKAGE_DB_FLAGS_CABAL
declare -a PACKAGE_DB_FLAGS_GHC

pkg_haskell_setup_env() {
	local pkg
	for dep in $DEPENDS; do
            pkg=$PREFIX/$(get_property $dep PACKAGE)

            [ -d "$pkg/bin" ]     && PATH="$pkg/bin:$PATH"
            [ -d "$pkg/include" ] && C_INCLUDE_PATH="$pkg/include:$C_INCLUDE_PATH"

            if [ -d "$pkg/package.conf.d" ]; then
                PACKAGE_DB_FLAGS_CABAL+=("--package-db=$pkg/package.conf.d")
                PACKAGE_DB_FLAGS_GHC+=("-package-db=$pkg/package.conf.d")
            fi

            if [ -e "$pkg/lib" ]; then
                LD_LIBRARY_PATH="$pkg/lib:$LD_LIBRARY_PATH"
                LIBRARY_PATH="$pkg/lib:$LIBRARY_PATH"
            fi
        done

        export PATH C_INCLUDE_PATH LD_LIBRARY_PATH LIBRARY_PATH
}

pkg_configure() {
	pkg_haskell_setup_env

	ghc -j --make "${PACKAGE_DB_FLAGS_GHC[@]}" Setup.*hs

        PACKAGE_DB="$PREFIX/$PACKAGE"/package.conf.d

        [[ ! -f "${PACKAGE_DB}/package.cache" ]] && ghc-pkg init "$PACKAGE_DB"

	./Setup configure \
                "${PACKAGE_DB_FLAGS_CABAL[@]}" \
		--package-db="$PACKAGE_DB" \
		--prefix="$PREFIX/$PACKAGE" \
		--enable-shared
}

pkg_build() {
	./Setup build "-j$SLOTS"
}

pkg_check() {
	true
}

pkg_install() {
	./Setup install
}

pkg_install_done() {
	touch "$PREFIX/$PACKAGE/.installed"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "${PACKAGE}.cabal" "$FILE"
}
