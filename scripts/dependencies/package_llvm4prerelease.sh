NAME="llvm4prerelease"
VERSION="master"
PACKAGE="$NAME-$VERSION"

DEPENDS="gcc cmake"

export CC="$PREFIX/gcc-latest/bin/gcc"
export CXX="$PREFIX/gcc-latest/bin/g++"
export PATH="$PREFIX/gcc-latest/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/gcc-latest/lib64"

pkg_download() {
	git clone "http://llvm.org/git/llvm.git" "$PACKAGE"
	git clone "http://llvm.org/git/clang.git" "$PACKAGE/llvm/tools/clang"
	git clone "http://llvm.org/git/compiler-rt.git" "$PACKAGE/llvm/projects/compiler-rt"
	git clone "http://llvm.org/git/openmp.git" "$PACKAGE/llvm/projects/openmp"
	git clone "http://llvm.org/git/libcxx.git" "$PACKAGE/llvm/projects/libcxx"
	git clone "http://llvm.org/git/libcxxabi.git" "$PACKAGE/llvm/projects/libcxxabi"
}

pkg_extract() {
	true
}

pkg_configure() {
	mkdir build
	cd build
	$PREFIX/cmake-latest/bin/cmake .. -DCMAKE_INSTALL_PREFIX="$PREFIX/$PACKAGE" -DCMAKE_BUILD_TYPE=Release
}

pkg_cleanup() {
	rm -rf "$PACKAGE"
}
