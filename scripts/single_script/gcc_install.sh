# setup environment variables
. ../environment.setup

VERSION=4.6.3
########################################################################
##                                                              GCC
########################################################################
echo "#### Downloading GCC ####"
wget http://gcc.igor.onlinedirect.bg/releases/gcc-$VERSION/gcc-$VERSION.tar.bz2
tar -xf gcc-$VERSION.tar.bz2

rm -Rf $PREFIX/gcc-$VERSION

echo "#### Building GCC ####"
mkdir gcc-build
cd gcc-build

LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

CFLAGS="-mtune=native" CXXFLAGS="-mtune=native" ../gcc-$VERSION/configure --prefix=$PREFIX/gcc-$VERSION --enable-languages=c,c++ --with-gmp=$PREFIX/gmp-latest --with-mpfr=$PREFIX/mpfr-latest --with-mpc=$PREFIX/mpc-latest --with-ppl=$PREFIX/ppl-latest --with-cloog=$PREFIX/cloog-gcc-latest --enable-cloog-backend=isl --disable-multilib  --enable-lto
make -j $SLOTS

echo "#### Installing GCC ####"
make install $PREFIX/gcc-$VERSION

rm $PREFIX/gcc-latest
ln -s $PREFIX/gcc-$VERSION $PREFIX/gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gcc-build
rm -R gcc-$VERSION*
