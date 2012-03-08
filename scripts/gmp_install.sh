# setup environment variables
. environment.setup

VERSION=5.0.4

########################################################################
##								LuaJIT
########################################################################
rm -Rf $PREFIX/gmp-$VERSION
echo "#### Downloading GMP library ####"
wget ftp://ftp.gmplib.org/pub/gmp-$VERSION/gmp-$VERSION.tar.bz2
tar -jxf gmp-$VERSION.tar.bz2
cd gmp-$VERSION

echo "#### Building GMP library ####"
./configure --prefix=$PREFIX/gmp-$VERSION
make -j $SLOTS

echo "#### Installing GMP library ####"
make install $PREFIX/gmp-$VERSION

rm $PREFIX/gmp-latest
ln -s $PREFIX/gmp-$VERSION $PREFIX/gmp-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gmp-$VERSION*

