# setup environment variables
. ../environment.setup

VERSION=0.35
########################################################################
##							BARVINOK	
########################################################################

rm -Rf $PREFIX/barvinok-$VERSION
echo "#### Downloading Barvinok library ####"
wget -nc http://www.kotnet.org/~skimo/barvinok/barvinok-$VERSION.tar.bz2
tar -xf barvinok-$VERSION.tar.bz2
cd barvinok-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/ntl-latest/lib:$LD_LIBRARY_PATH 

CFLAGS="-mtune=native -O3 -fPIC"
CXXFLAGS=$CFLAGS
echo "#### Building Barvinok library ####"
CC=$CC CXX=$CXX CXXFLAGS=$CFLAGS CFLAGS=$CFLAGS ./configure --prefix=$PREFIX/barvinok-$VERSION --with-libgmp=$PREFIX/gmp-latest --with-ntl=$PREFIX/ntl-latest --with-isl=system --with-isl-prefix=$PREFIX/isl-latest --with-cloog=no --enable-shared-barvinok
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Barvinok library ####"
make install

rm $PREFIX/barvinok-latest
ln -s $PREFIX/barvinok-$VERSION $PREFIX/barvinok-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf barvinok-$VERSION*

exit 0
