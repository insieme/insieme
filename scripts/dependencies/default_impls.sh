# This file contains the default implementation for install step.s

pkg_download() {
	wget -nc "$URL"
	if [[ "$SHA256SUM" ]]; then
		echo "$SHA256SUM  $FILE" | sha256sum -c
	fi
}

pkg_extract() {
	tar xf "$FILE"
}

pkg_prepare() {
	find "$INSTALLER_DIR/patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -N -i
}

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE"
}

pkg_build() {
	make -j "$SLOTS"
}

pkg_check() {
	true
}

pkg_install() {
	make install
}

pkg_install_done() {
	touch "$PREFIX/$PACKAGE/.installed"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$FILE"
}
