# setup environment variables
. ../environment.setup

VERSION=4.6.3
########################################################################
##                                                              GCC
########################################################################
rm -Rf $PREFIX/gcc-$VERSION
echo "#### Downloading GCC ####"
wget http://gcc.igor.onlinedirect.bg/releases/gcc-$VERSION/gcc-$VERSION.tar.gz
wget http://gcc.igor.onlinedirect.bg/releases/gcc-$VERSION/gcc-g++-$VERSION.tar.gz

tar -xzf gcc-$VERSION.tar.gz
tar -xzf gcc-g++-$VERSION.tar.gz

echo "#### Building GCC ####"
mkdir gcc-build
cd gcc-build

LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

../gcc-$VERSION/configure --prefix=$PREFIX/gcc-$VERSION --enable-languages=c,c++ --with-gmp=$PREFIX/gmp-latest --with-mpfr=$PREFIX/mpfr-latest --with-mpc=$PREFIX/mpc-latest
make -j $SLOTS

echo "#### Installing GCC ####"
make install $PREFIX/gcc-$VERSION

rm $PREFIX/gcc-latest
ln -s $PREFIX/gcc-$VERSION $PREFIX/gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gcc-build
rm -R gcc-$VERSION*
rm -R gcc-g++-$VERSION*
