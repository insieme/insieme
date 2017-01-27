NAME="llvm4prerelease"
VERSION="master"
PACKAGE="$NAME-$VERSION"

DEPENDS="gcc cmake python"

GCC_PKG=$(get_property gcc PACKAGE)
CMAKE_PKG=$(get_property cmake PACKAGE)

export CC="$PREFIX/$GCC_PKG/bin/gcc"
export CXX="$PREFIX/$GCC_PKG/bin/g++"
export PATH="$PREFIX/$GCC_PKG/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/$GCC_PKG/lib64"

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
}

pkg_cleanup() {
	rm -rf "$PACKAGE"
}
