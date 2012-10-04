# setup environment variables
. ./environment.setup

VERSION=3.1.8
CFLAGS="-mtune=native -O3 -fgraphite-identity"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3 -fgraphite-identity"

########################################################################
##								CCache
########################################################################
rm -Rf $PREFIX/ccache-$VERSION
echo "#### Downloading CCache ####"
wget -nc http://samba.org/ftp/ccache/ccache-$VERSION.tar.bz2

if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf ccache-$VERSION.tar.bz2
cd ccache-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building CCache ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/ccache-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing CCache ####"
make install 

rm $PREFIX/ccache-latest
ln -s $PREFIX/ccache-$VERSION $PREFIX/ccache-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R ccache-$VERSION*

exit 0
