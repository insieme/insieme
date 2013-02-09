# setup environment variables
. ./environment.setup

VERSION=0.11.1

OPT_FLAGS="-mtune=native -O3"

########################################################################
##							ISL
########################################################################

rm -Rf $PREFIX/isl-$VERSION
echo "#### Downloading isl library ####"
wget -nc ftp://ftp.linux.student.kuleuven.be/pub/people/skimo/isl/isl-$VERSION.tar.bz2

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf isl-$VERSION.tar.bz2
cd isl-$VERSION

echo "#### Building isl library ####"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

CC=$CC CXX=$CXX CFLAGS=$OPT_FLAGS LDFLAGS=$OPT_FLAGS ./configure --prefix=$PREFIX/isl-$VERSION \
	--with-gmp=system \
	--with-gmp-prefix=$PREFIX/gmp-latest

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing isl library ####"
make install

rm $PREFIX/isl-latest
ln -s $PREFIX/isl-$VERSION $PREFIX/isl-latest
# symlink necessary for mach.uibk.ac.at
ln -s libisl.so $PREFIX/isl-latest/lib/libisl.so.5

echo "#### Cleaning up environment ####"
cd ..
rm -Rf isl-$VERSION*

exit 0
