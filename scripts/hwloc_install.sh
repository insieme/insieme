# setup environment variables
. ./environment.setup

VERSION=1.5
CFLAGS="-mtune=native -O3 -fgraphite-identity"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3 -fgraphite-identity"

########################################################################
##								HWLOC
########################################################################
rm -Rf $PREFIX/ccache-$VERSION
echo "#### Downloading HWLOC ####"
wget -nc http://www.open-mpi.org/software/hwloc/v$VERSION/downloads/hwloc-$VERSION.tar.gz

if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf hwloc-$VERSION.tar.gz
cd hwloc-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building CCache ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/hwloc-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing CCache ####"
make install 

rm $PREFIX/hwloc-latest
ln -s $PREFIX/hwloc-$VERSION $PREFIX/hwloc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R hwloc-$VERSION*

exit 0
