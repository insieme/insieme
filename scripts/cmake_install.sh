# setup environment variables
. ./environment.setup

VER=3.2
VERSION=3.2.1

########################################################################
##							CMake	
########################################################################

if [ -d $PREFIX/cmake-$VERSION ]; then
  echo "CMake version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/cmake-$VERSION

CFLAGS="-O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-O3"

echo "#### Downloading CMake library ####"
wget -nc http://www.cmake.org/files/v$VER/cmake-$VERSION.tar.gz

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH 
export LD_RUN_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/gcc-latest/lib64

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf cmake-$VERSION.tar.gz
cd cmake-$VERSION

echo "#### Building CMake library ####"
CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/cmake-$VERSION 
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing CMake library ####"
make install 

rm -f $PREFIX/cmake-latest
ln -s $PREFIX/cmake-$VERSION $PREFIX/cmake-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R cmake-$VERSION*

exit 0
