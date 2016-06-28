# setup environment variables
. ./environment.setup

########################################################################
##                                 GCC
########################################################################

VERSION=5.1.0 #5.2.0, 6.1.0
PACKAGE=gcc-$VERSION
FILE=gcc-$VERSION.tar.gz

if [ -n "$NEW_CXX" ]
then
	if [ -x "`which ${NEW_CXX}`" -a `${NEW_CXX} --version | head -n1 |  sed 's/^.* //g'` = ${VERSION} ]
	then
	    echo "GCC version $VERSION already installed - using `which ${NEW_CXX}`"
	    exit 0
	fi
fi

echo "#### Downloading GCC ####"
wget -nc http://www.insieme-compiler.org/ext_libs/$FILE

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Removing old build folder ####"
rm -Rf gcc-build

if [ ! -d "$PACKAGE" ]; then
	echo "#### Unpacking tar ####"
	tar -xf $FILE
fi

echo "#### Building GCC ####"
mkdir gcc-build
cd gcc-build

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH

export LD_RUN_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib

CFLAGS="-mtune=native -O3" ../$PACKAGE/configure \
		--prefix=$PREFIX/gcc-$VERSION \
		--enable-languages=c,c++ \
		--with-gmp=$PREFIX/gmp-latest \
		--with-mpfr=$PREFIX/mpfr-latest \
		--with-mpc=$PREFIX/mpc-latest \
		--without-isl \
		--disable-multilib  \
		--enable-lto

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

