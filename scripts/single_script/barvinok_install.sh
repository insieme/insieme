# setup environment variables
. ../environment.setup

VERSION=0.35
########################################################################
##							BARVINOK	
########################################################################

rm -Rf $PREFIX/barvinok-$VERSION
echo "#### Downloading Barvinok library ####"
wget http://www.kotnet.org/~skimo/barvinok/barvinok-$VERSION.tar.bz2
tar -xf barvinok-$VERSION.tar.bz2
cd barvinok-$VERSION

echo "#### Building Barvinok library ####"
./configure --prefix=$PREFIX/barvinok-$VERSION --with-libgmp=$PREFIX/gmp-latest --with-ntl=$PREFIX/ntl-latest --with-isl=system --with-isl-prefix=$PREFIX/isl-latest --with-cloog=no --enable-shared-barvinok
make -j $SLOTS

echo "#### Installing Barvinok library ####"
make install

rm $PREFIX/barvinok-latest
ln -s $PREFIX/barvinok-$VERSION $PREFIX/barvinok-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf barvinok-$VERSION*

