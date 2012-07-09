# setup environment variables
. ./environment.setup

VERSION=2.2.6b

########################################################################
##								MPC
########################################################################
rm -Rf $PREFIX/libtool-$VERSION
echo "#### Downloading libtool library ####"
wget -nc http://ftpmirror.gnu.org/libtool/libtool-$VERSION.tar.gz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

tar -xzf libtool-$VERSION.tar.gz
cd libtool-$VERSION

echo "#### Building libtool library ####"
CC=$CC CXX=$CXX CFLAGS="-mtune=native -O3" CXXFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/libtool-$VERSION 
make -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing libtool library ####"
make install

rm $PREFIX/libtool-latest
ln -s $PREFIX/libtool-$VERSION $PREFIX/libtool-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf libtool-$VERSION*

exit 0
