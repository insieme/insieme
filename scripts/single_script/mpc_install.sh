# setup environment variables
. ../environment.setup

VERSION=0.9

########################################################################
##								MPC
########################################################################
rm -Rf $PREFIX/mpc-$VERSION
echo "#### Downloading MPC library ####"
wget http://www.multiprecision.org/mpc/download/mpc-$VERSION.tar.gz
tar -xzf mpc-$VERSION.tar.gz
cd mpc-$VERSION

echo "#### Building MPC library ####"
CFLAGS="-m64 -O3" LDFLAGS="-m64 -O3" CXXFLAGS="-m64 -O3" ./configure --prefix=$PREFIX/mpc-$VERSION --with-gmp=$PREFIX/gmp-latest --with-mpfr=$PREFIX/mpfr-latest
make -j $SLOTS
make check

echo "#### Installing MPC library ####"
make install

rm $PREFIX/mpc-latest
ln -s $PREFIX/mpc-$VERSION $PREFIX/mpc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R mpc-$VERSION*

