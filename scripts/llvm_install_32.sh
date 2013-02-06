# setup environment variables
. ./environment.setup

###########################################################################
#  instalation notes:  
#  ==================
#
#   make sure which llvm llvm-latest is pointing to
#   libLLVM-3.2.so may not exist after compilation    
#             create an alias to libLLVM-3.2svn.so in $PREFIX/llvm-3.2/libs
###########################################################################

VERSION=3.2

rm -R $PREFIX/llvm-$VERSION

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget -nc http://llvm.org/releases/$VERSION/llvm-$VERSION.src.tar.gz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf llvm-$VERSION.src.tar.gz
# change dire into tools
cd llvm-$VERSION.src/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget -nc http://llvm.org/releases/$VERSION/clang-$VERSION.src.tar.gz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf clang-$VERSION.src.tar.gz
mv clang-$VERSION.src clang
rm -f clang-$VERSION.src.tar.gz
cd ../

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
patch -p1  < ../patches/insieme-clang-$VERSION.patch

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/mpc-latest/lib/:$LD_LIBRARY_PATH 

CFLAGS="-mtune=native -O3 -fgraphite-identity -std=c++0x"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CFLAGS LDFLAGS="-mtune=native -O3 -std=c++0x" ../llvm-$VERSION.src/configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes\
  	 --enable-assert=yes --enable-debug-runtime=no --enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-$VERSION.src
rm -f llvm-$VERSION.src.tar.gz


#echo "****************************************************************"
#echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
#echo "****************************************************************"
patch -d $PREFIX/llvm-$VERSION/lib/clang/$VERSION/include < ./patches/stdarg.patch

#rm -f $PREFIX/llvm-latest
#ln -s $PREFIX/llvm-$VERSION $PREFIX/llvm-latest
ln -s $PREFIX/llvm-$VERSION/lib/libLLVM-3.2svn.so $PREFIX/llvm-$VERSION/lib/libLLVM-3.2.so


exit 0
