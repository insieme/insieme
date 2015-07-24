# setup environment variables
. ./environment.setup

VERSION=2.3.4

########################################################################
##                                                      SHARK 
########################################################################

if [ -d $PREFIX/shark-$VERSION ]; then
  echo "SHARK version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/shark-$VERSION
echo "#### Downloading SHARK library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/shark-$VERSION.zip

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

unzip shark-$VERSION.zip
cd Shark 

# patch shark
patch -p0 < ../patches/svm.cpp.patch
patch -p0 < ../patches/svm.h.patch
patch -p0 < ../patches/fileutil.patch
patch -p0 < ../patches/randomvector.patch

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building SHARK library ####"
mkdir build
cd build
CC=$CC CXX=$CXX XFLAGS="-mtune=native -O3" $PREFIX/cmake-latest/bin/cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX/shark-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing SHARK library ####"
make install 

rm -f $PREFIX/shark-latest
ln -s $PREFIX/shark-$VERSION $PREFIX/shark-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -R Shark
rm -R shark-$VERSION* 

exit 0
