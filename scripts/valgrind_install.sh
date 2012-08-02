# setup environment variables
. ./environment.setup

VERSION=3.7.0

CFLAGS="-O3 -mtune=native -fgraphite-identity -fPIC"
LDFLAGS="-O3 -mtune=native -fPIC"

########################################################################
##								Valgrind
########################################################################
rm -Rf $PREFIX/valgrind-$VERSION

echo "#### Downloading Valgrind ####"
wget http://valgrind.org/downloads/valgrind-$VERSION.tar.bz2

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar xvf valgrind-$VERSION.tar.bz2
cd valgrind-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 


echo "#### Configuring Valgrind ####"
./configure --prefix=$PREFIX/valgrind-$VERSION

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi


echo "#### Building Valgrind ####"
CFLAGS=$CFLAGS LDFLAGS=$LDFLAGS PREFIX=$PREFIX/valgrind-$VERSION make CC=$CC -j$SLOTS

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Valgrind ####"
make install

rm -f $PREFIX/valgrind-latest
ln -s $PREFIX/valgrind-$VERSION $PREFIX/valgrind-latest


echo "#### Cleaning up ####"
cd ..
rm -R valgrind-$VERSION*

exit 0
