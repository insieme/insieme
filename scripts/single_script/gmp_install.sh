# setup environment variables
#. ../environment.setup

VERSION=5.0.4

########################################################################
##								GMP
########################################################################
rm -Rf $PREFIX/gmp-$VERSION
echo "#### Downloading GMP library ####"
wget ftp://ftp.gmplib.org/pub/gmp-$VERSION/gmp-$VERSION.tar.bz2
tar -jxf gmp-$VERSION.tar.bz2
cd gmp-$VERSION

echo "#### Building GMP library ####"
CFLAGS="-m64 -O3" LDFLAGS="-m64 -O3" CXXFLAGS="-m64 -O3" ./configure --prefix=$PREFIX/gmp-$VERSION --enable-cxx
make -j $SLOTS
make check

echo "#### Installing GMP library ####"
make install 

rm $PREFIX/gmp-latest
ln -s $PREFIX/gmp-$VERSION $PREFIX/gmp-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gmp-$VERSION*

