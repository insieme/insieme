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
CFLAGS="-mtune=native -O3" LDFLAGS="-mtune=native -O3" CXXFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/gmp-$VERSION --enable-cxx
make -j $SLOTS
make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing GMP library ####"
make install 

rm $PREFIX/gmp-latest
ln -s $PREFIX/gmp-$VERSION $PREFIX/gmp-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gmp-$VERSION*

exit 0

