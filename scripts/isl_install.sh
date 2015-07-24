# setup environment variables
. ./environment.setup

VERSION=0.10

OPT_FLAGS="-mtune=native -O3"

########################################################################
##							ISL
########################################################################

if [ -d $PREFIX/isl-$VERSION ]; then
  echo "ISL version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/isl-$VERSION
echo "#### Downloading isl library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/isl-${VERSION}.tar.gz

tar -xf isl-${VERSION}.tar.gz
rm -f isl-${VERSION}.tar.gz

cd isl-$VERSION
./autogen.sh

echo "#### Building isl library ####"

#export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

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
