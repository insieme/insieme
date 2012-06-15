# setup environment variables
. ../environment.setup

VERSION=0.17.0
########################################################################
##							CLOOG	
########################################################################

rm -Rf $PREFIX/cloog-$VERSION
echo "#### Downloading Cloog library ####"
wget -nc http://www.bastoul.net/cloog/pages/download/count.php3?url=./cloog-$VERSION.tar.gz -O cloog-$VERSION.tar.gz
tar -xf cloog-$VERSION.tar.gz
cd cloog-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Cloog library ####"
CC=$CC CXX=$CXX CFLAGS="-mtune=native -O3" LDFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/cloog-$VERSION --with-gmp=system --with-gmp-prefix=$PREFIX/gmp-latest --with-isl=system --with-isl-prefix=$PREFIX/isl-latest
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Cloog library ####"
make install

rm $PREFIX/cloog-latest
ln -s $PREFIX/cloog-$VERSION $PREFIX/cloog-latest
# symlink necessary for mach.uibk.ac.at
ln -s libcloog-isl.so $PREFIX/cloog-latest/lib/libcloog-isl.so.1

echo "#### Cleaning up environment ####"
cd ..
rm -Rf cloog-$VERSION*

exit 0
