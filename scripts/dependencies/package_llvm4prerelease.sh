NAME="llvm4prerelease"
VERSION="master"
PACKAGE="$NAME-$VERSION"

DEPENDS="gcc cmake python"

export CC="$PREFIX/gcc-latest/bin/gcc"
export CXX="$PREFIX/gcc-latest/bin/g++"
export PATH="$PREFIX/gcc-latest/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/gcc-latest/lib64"

pkg_download() {
	git clone "http://llvm.org/git/llvm.git" "$PACKAGE"
	git clone "http://llvm.org/git/clang.git" "$PACKAGE/tools/clang"
	git clone "http://llvm.org/git/compiler-rt.git" "$PACKAGE/projects/compiler-rt"
	git clone "http://llvm.org/git/openmp.git" "$PACKAGE/projects/openmp"
	git clone "http://llvm.org/git/libcxx.git" "$PACKAGE/projects/libcxx"
	git clone "http://llvm.org/git/libcxxabi.git" "$PACKAGE/projects/libcxxabi"
}

pkg_extract() {
	true
}

pkg_configure() {
	mkdir build
	cd build
	$PREFIX/cmake-latest/bin/cmake \
		-DCMAKE_INSTALL_PREFIX="$PREFIX/$PACKAGE" \
		-DCMAKE_BUILD_TYPE=Release \
		-DPYTHON_EXECUTABLE="$PREFIX/python-latest/bin/python" \
		..
}

pkg_build() {
	make -j "$SLOTS"
	make -j "$SLOTS" cxx
}

pkg_install() {
	make install
	make install-cxx install-cxxabi

	rm -f "$PREFIX/$NAME-latest"
	ln -s "$PREFIX/$PACKAGE" "$PREFIX/$NAME-latest"
	touch "$PREFIX/$PACKAGE/.installed"
}

pkg_cleanup() {
	rm -rf "$PACKAGE"
}
