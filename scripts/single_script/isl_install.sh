# setup environment variables
. ../environment.setup

VERSION=0.09
########################################################################
##							ISL
########################################################################

rm -Rf $PREFIX/isl-$VERSION
echo "#### Downloading isl library ####"
wget http://www.kotnet.org/~skimo/isl/isl-$VERSION.tar.bz2
tar -xf isl-$VERSION.tar.bz2
cd isl-$VERSION

echo "#### Building isl library ####"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

CC=$CC CXX=$CXX CFLAGS="-mtune=native -O3" CXXFLAGS="-O3 -mtune=native" LDFLAGS="-O3" ./configure --prefix=$PREFIX/isl-$VERSION --with-gmp=system --with-gmp-prefix=$PREFIX/gmp-latest
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ] then
	exit $RET
fi

echo "#### Installing isl library ####"
make install

rm $PREFIX/isl-latest
ln -s $PREFIX/isl-$VERSION $PREFIX/isl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf isl-$VERSION*

exit 0
