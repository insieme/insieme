# setup environment variables
. environment.setup

VER=2.8
VERSION=2.8.7

########################################################################
##							CMake	
########################################################################
rm -Rf $PREFIX/cmake-$VERSION
echo "#### Downloading CMake library ####"
wget http://www.cmake.org/files/v$VER/cmake-$VERSION.tar.gz
tar -xzf cmake-$VERSION.tar.gz
cd cmake-$VERSION

echo "#### Building CMake library ####"
./configure --prefix=$PREFIX/cmake-$VERSION 
make -j $SLOTS

echo "#### Installing CMake library ####"
make install 

rm $PREFIX/cmake-latest
ln -s $PREFIX/cmake-$VERSION $PREFIX/cmake-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R cmake-$VERSION*
