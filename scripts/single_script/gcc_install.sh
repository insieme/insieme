# setup environment variables
. ../environment.setup

########################################################################
##                                 GCC
########################################################################

VERSION=4.6.3
PACKAGE=gcc-$VERSION
FILE=gcc-$VERSION.tar.bz2

echo "#### Downloading GCC ####"
wget -nc http://gcc.igor.onlinedirect.bg/releases/gcc-$VERSION/$FILE

echo "#### Removing old build folder ####"
rm -Rf gcc-build

if [ ! -d "$PACKAGE" ]; then
	echo "#### Unpacking tar ####"
	tar -xf $FILE
fi

echo "#### Building GCC ####"
mkdir gcc-build
cd gcc-build

LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH

export LD_LIBRARY_PATH

CFLAGS="-mtune=native -O3" ../$PACKAGE/configure \
		--prefix=$PREFIX/gcc-$VERSION \
		--enable-languages=c,c++ \
		--with-gmp=$PREFIX/gmp-latest \
		--with-mpfr=$PREFIX/mpfr-latest \
		--with-mpc=$PREFIX/mpc-latest \
		--with-ppl=$PREFIX/ppl-latest \
		--with-cloog=$PREFIX/cloog-gcc-latest \
		--enable-cloog-backend=isl \
		--disable-multilib  \
		--enable-lto \
	    --with-boot-ldflags="-L$PREFIX/gmp-latest/lib -L$PREFIX/mpfr-latest/lib -L$PREFIX/mpc-latest/lib -L$PREFIX/cloog-gcc-latest/lib -L$PREFIX/ppl-latest/lib"
		--with-stage1-ldflags="-L$PREFIX/gmp-latest/lib -L$PREFIX/mpfr-latest/lib -L$PREFIX/mpc-latest/lib -L$PREFIX/cloog-gcc-latest/lib -L$PREFIX/ppl-latest/lib"

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing GCC ####"
make install

rm $PREFIX/gcc-latest
ln -s $PREFIX/$PACKAGE $PREFIX/gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R gcc-build
rm -R $PACKAGE*

exit 0

