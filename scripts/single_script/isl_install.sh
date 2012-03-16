# setup environment variables
. ../environment.setup

VERSION=0.09
########################################################################
##							ISL
########################################################################

rm -Rf $PREFIX/isl-$VERSION
echo "#### Downloading isl library ####"
wget http://www.kotnet.org/~skimo/isl/isl-$VERSION.tar.bz2
tar -xf isl-$VERSION.tar.bz2
cd isl-$VERSION

echo "#### Building isl library ####"
./configure --prefix=$PREFIX/isl-$VERSION --with-gmp=system --with-gmp-prefix=$PREFIX/gmp-latest
make -j $SLOTS

echo "#### Installing isl library ####"
make install

rm $PREFIX/isl-latest
ln -s $PREFIX/isl-$VERSION $PREFIX/isl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf isl-$VERSION*

