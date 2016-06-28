# setup environment variables
. ./environment.setup
set -e

###########################################################################
#  instalation notes:  
###########################################################################

VERSION=3.6.2

if [ -d $PREFIX/llvm-$VERSION ]; then
  echo "LLVM version $VERSION already installed"
  exit 0
fi

CURRENT=`pwd`

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget -nc http://www.insieme-compiler.org/ext_libs/llvm-$VERSION.src.tar.xz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf llvm-$VERSION.src.tar.xz
# change dire into tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"

wget -nc http://www.insieme-compiler.org/ext_libs/cfe-$VERSION.src.tar.xz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

cd llvm-$VERSION.src/tools

tar -xf ../../cfe-$VERSION.src.tar.xz

mv cfe-$VERSION.src clang
cd $CURRENT

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
cd llvm-$VERSION.src
patch -p1  < $CURRENT/patches/insieme-clang-$VERSION.patch

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH

CFLAGS="-O3 -std=c++0x"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CFLAGS LDFLAGS="-mtune=native -O3" \
	$CURRENT/llvm-$VERSION.src/configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes\
  	 --enable-assert=yes --enable-debug-runtime=no --enable-debug-symbols=no --enable-optimized=yes --enable-bindings=none
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	echo " compilation failed "
	exit $RET
fi

#echo "*******************************************"
#echo "* Removing old compilation and installing "
#echo "*******************************************"
make clang-only install -j$SLOTS

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-$VERSION.src
rm -f llvm-$VERSION.src.tar.xz
rm -f cfe-$VERSION.src.tar.xz


rm -f $PREFIX/llvm-latest
ln -s $PREFIX/llvm-$VERSION $PREFIX/llvm-latest

exit 0
