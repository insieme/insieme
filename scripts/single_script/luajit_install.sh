# setup environment variables
. ../environment.setup

VERSION=2.0.0-beta9

CFLAGS="-O3 -mtune=native -fgraphite-identity -fPIC"
LDFLAGS="-O3 -mtune=native -fPIC"

########################################################################
##								LuaJIT
########################################################################
rm -Rf $PREFIX/luajit-$VERSION
echo "#### Downloading LuaJIT library ####"
wget http://luajit.org/download/LuaJIT-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf LuaJIT-$VERSION.tar.gz
cd LuaJIT-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building LuaJIT library ####"
CFLAGS=$CFLAGS LDFLAGS=$LDFLAGS PREFIX=$PREFIX/luajit-$VERSION make CC=$CC -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing LuaJIT library ####"
make install PREFIX=$PREFIX/luajit-$VERSION

rm -f $PREFIX/luajit-latest
ln -s $PREFIX/luajit-$VERSION $PREFIX/luajit-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R LuaJIT-$VERSION*

exit 0
