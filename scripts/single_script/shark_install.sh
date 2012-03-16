# setup environment variables
. ../environment.setup

VERSION=2.3.4

########################################################################
##                                                      SHARK 
########################################################################
rm -Rf $PREFIX/shark-$VERSION
echo "#### Downloading SHARK library ####"
wget http://sourceforge.net/projects/shark-project/files/Shark%20Core/Shark%20$VERSION/shark-$VERSION.zip/download

unzip shark-$VERSION.zip
cd Shark 

echo "#### Building SHARK library ####"
mkdir build
cd build
$PREFIX/cmake-latest/bin/cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX/shark-$VERSION
make -j $SLOTS

echo "#### Installing SHARK library ####"
make install 

rm $PREFIX/shark-latest
ln -s $PREFIX/shark-$VERSION $PREFIX/shark-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -R Shark
rm -R shark-$VERSION* 
