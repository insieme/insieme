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
CFLAGS="-mtune=native -O3" LDFLAGS="-mtune=native -O3" CXXFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/mpc-$VERSION --with-gmp=$PREFIX/gmp-latest --with-mpfr=$PREFIX/mpfr-latest
make -j $SLOTS
make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing MPC library ####"
make install

rm $PREFIX/mpc-latest
ln -s $PREFIX/mpc-$VERSION $PREFIX/mpc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R mpc-$VERSION*

exit 0
