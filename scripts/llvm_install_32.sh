# setup environment variables
. ./environment.setup

VERSION=3.2

rm -R $PREFIX/llvm-$VERSION-src

CURRENT=`pwd`

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM version *"
echo "*****************************************"

cd $PREFIX

svn co http://llvm.org/svn/llvm-project/llvm/branches/release_32/ llvm-$VERSION-src

cd llvm-$VERSION-src/tools
svn co http://llvm.org/svn/llvm-project/cfe/branches/release_32/ clang
cd ../..

cd llvm-$VERSION-src/tools/clang/tools
svn co http://llvm.org/svn/llvm-project/clang-tools-extra/branches/release_32/ extra
cd ../../../..

cd llvm-$VERSION-src/projects
svn co http://llvm.org/svn/llvm-project/compiler-rt/branches/release_32 compiler-rt
cd ../..

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"

patch -d llvm-$VERSION-src -p1  < $CURRENT/patches/insieme-clang-$VERSION.patch


echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"

mkdir $PREFIX/build 
cd $PREFIX/build


echo `pwd`

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/mpc-latest/lib/:$LD_LIBRARY_PATH 

CFLAGS="-mtune=native -O3 -fgraphite-identity"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CFLAGS LDFLAGS="-mtune=native -O3" ../llvm-$VERSION-src/configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes\
  	 --enable-assert=yes --enable-debug-runtime=no --enable-debug-symbols=no --enable-optimized=yes

make REQUIRES_RTTI=1 -j$SLOTS

## Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

make install

cd ..
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"

rm -r build llvm-$VERSION-src

exit 0
