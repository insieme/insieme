# setup environment variables
. environment.setup

VERSION=3.1.1

########################################################################
##		xerces
########################################################################
rm -Rf $PREFIX/xerces-$VERSION
echo "#### Downloading Xerces library ####"
wget http://mirror.sti2.at/apache//xerces/c/3/sources/xerces-c-$VERSION.tar.gz
tar -xzf xerces-c-$VERSION.tar.gz
cd xerces-c-$VERSION

echo "#### Building Xerces library ####"
./configure CFLAGS=-O3 CXXFLAGS=-O3 --prefix=$PREFIX/xerces-$VERSION
cd src
make -j$SLOTS

echo "#### Installing Xerces library ####"
make install
cd ..

ln -sf $PREFIX/xerces-$VERSION $PREFIX/xerces-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R xerces-c-$VERSION*




