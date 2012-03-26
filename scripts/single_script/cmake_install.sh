# setup environment variables
. ../environment.setup

VER=2.8
VERSION=2.8.7

########################################################################
##							CMake	
########################################################################
rm -Rf $PREFIX/cmake-$VERSION

CFLAGS="-mtune=native -O3 -fgraphite-identity"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Downloading CMake library ####"
wget http://www.cmake.org/files/v$VER/cmake-$VERSION.tar.gz
tar -xzf cmake-$VERSION.tar.gz
cd cmake-$VERSION

echo "#### Building CMake library ####"
CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/cmake-$VERSION 
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ] then
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
