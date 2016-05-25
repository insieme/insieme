# setup environment variables
. ./environment.setup

VERSION=3.11.0

CFLAGS="-O3 -fPIC"
LDFLAGS="-O3 -fPIC"

########################################################################
##								Valgrind
########################################################################
rm -Rf $PREFIX/valgrind-$VERSION

echo "#### Downloading Valgrind ####"
wget -nc http://www.insieme-compiler.org/ext_libs/valgrind-$VERSION.tar.bz2

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar xf valgrind-$VERSION.tar.bz2
cd valgrind-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH 


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
