# setup environment variables
. ./environment.setup

VERSION=1.7.0

CFLAGS="-O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-O3"

########################################################################
##                             Google Test
########################################################################

if [ -d $PREFIX/gtest-$VERSION ]; then
  echo "Google Test version $VERSION already installed"
  exit 0
fi

echo "#### Downloading Google Test library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/gtest-$VERSION.zip

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

unzip gtest-$VERSION.zip
cd gtest-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Google Test library ####"
mkdir build
cd build
CC=$CC CXX=$CXX LDFLAGS=$LDFLAGS CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS $PREFIX/cmake-latest/bin/cmake ../
make gtest
make gtest_main

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

cd ..

echo "#### Installing Google Test library ####"
rm -Rf $PREFIX/gtest-$VERSION
mkdir $PREFIX/gtest-$VERSION
cp -r ./include $PREFIX/gtest-$VERSION
mkdir $PREFIX/gtest-$VERSION/lib
cp build/*.a $PREFIX/gtest-$VERSION/lib

rm -f $PREFIX/gtest-latest
ln -s $PREFIX/gtest-$VERSION $PREFIX/gtest-latest
rm -f $PREFIX/gtest_main-latest
ln -s $PREFIX/gtest-$VERSION $PREFIX/gtest_main-latest

echo "#### Cleaning up environment ####"
cd ..
rm -rf gtest-$VERSION*

echo "#### Installation complete ####"

exit 0
