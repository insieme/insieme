# setup environment variables
. ./environment.setup

VERSION=1.4.1p1

CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Mpich2	
########################################################################
rm -Rf $PREFIX/mpich2-$VERSION
echo "#### Downloading Mpich2 ####"
wget -nc http://www.mcs.anl.gov/research/projects/mpich2/downloads/tarballs/$VERSION/mpich2-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

export LD_LIBRARY_PATH=./mpich2-$VERSION/lib:$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

tar -xzf mpich2-$VERSION.tar.gz
cd mpich2-$VERSION

echo "#### Building Mpich ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure -prefix=$PREFIX/mpich2-$VERSION --with-device=ch3:nemesis --disable-fc --disable-f77
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Mpich ####"
make install 

rm $PREFIX/mpich-latest
ln -s $PREFIX/mpich2-$VERSION $PREFIX/mpich-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R mpich2-$VERSION*

exit 0
