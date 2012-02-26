# setup environment variables
. environment.setup

VERSION=1.6.0

########################################################################
##                             Google Test
########################################################################
echo "#### Downloading Google Test library ####"
wget http://googletest.googlecode.com/files/gtest-$VERSION.zip
unzip gtest-$VERSION.zip
cd gtest-$VERSION

echo "#### Building Google Test library ####"
mkdir build
cd build
cmake ../
make gtest
make gtest_main
cd ..

echo "#### Installing Google Test library ####"
rm -Rf $PREFIX/gtest-$VERSION
mkdir $PREFIX/gtest-$VERSION
cp -r ./include $PREFIX/gtest-$VERSION
mkdir $PREFIX/gtest-$VERSION/lib
cp build/*.a $PREFIX/gtest-$VERSION/lib

ln -sf $PREFIX/gtest-$VERSION $PREFIX/gtest-latest
ln -sf $PREFIX/gtest-$VERSION $PREFIX/gtest_main-latest

echo "#### Cleaning up environment ####"
cd ..
rm -rf gtest-$VERSION*

echo "#### Installation complete ####"




