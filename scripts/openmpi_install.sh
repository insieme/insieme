# setup environment variables
. ./environment.setup

VER=1.6
VERSION=1.6.1

CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Openmpi	
########################################################################
rm -Rf $PREFIX/openmpi-$VERSION
echo "#### Downloading Openmpi ####"
wget -nc http://www.open-mpi.org/software/ompi/v$VER/downloads/openmpi-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

tar -xzf openmpi-$VERSION.tar.gz
cd openmpi-$VERSION

echo "#### Building Openmpi ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/openmpi-$VERSION --disable-mpi-f77 --disable-mpi-f90 --enable-mpi-thread-multiple --with-openib  --with-hwloc=$PREFIX/hwloc-latest --with-libltdl=$PREFIX/libtool-latest/
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Openmpi ####"
make install 

rm $PREFIX/openmpi-latest
ln -s $PREFIX/openmpi-$VERSION $PREFIX/openmpi-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R openmpi-$VERSION*

exit 0
