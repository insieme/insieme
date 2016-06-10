# default location
export PREFIX="${PREFIX:-$HOME/libs}"

# default compile flags
export CFLAGS="-mtune=native -O3"
export CXXFLAGS="-mtune=native -O3"
export LDLAGS="-mtune=native -O3"

# default compiler (system)
#export CC="$PREFIX/gcc-latest/bin/gcc"
#export CXX="$PREFIX/gcc-latest/bin/g++"
#export LD_LIBRARY_PATH="$PREFIX/gcc-latest/lib64"

# parallel build
export SLOTS="${SLOTS:-$(nproc)}"

pkg_check_installed() {
	if [[ -d "$PREFIX/$PACKAGE" ]]; then
		echo "$NAME $VERSION already installed"
		exit 0
	fi
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
	find "../patches" -name "$NAME-*.patch" | sort | xargs -r -L 1 patch -p1 -N -i
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
	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
}

pkg_cleanup() {
	rm -rf "$PACKAGE" "$FILE"
}
