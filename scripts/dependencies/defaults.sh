# default location
export PREFIX="${PREFIX:-$HOME/third_party_libs}"

# default compile flags
export CFLAGS="-mtune=native -O3"
export CXXFLAGS="-mtune=native -O3"
export LDLAGS="-mtune=native -O3"

# override compiler
#GCC_PKG=$(get_property gcc PACKAGE)
#export CC="$PREFIX/$GCC_PKG/bin/gcc"
#export CXX="$PREFIX/$GCC_PKG/bin/g++"
#export PATH="$PREFIX/$GCC_PKG/bin:$PATH"
#export LD_LIBRARY_PATH="$PREFIX/$GCC_PKG/lib64"

# parallel build
export SLOTS="${SLOTS:-$(nproc)}"

pkg_is_installed() {
	[[ -f "$PREFIX/$PACKAGE/.installed" ]]
}

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
