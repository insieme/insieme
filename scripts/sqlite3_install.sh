# setup environment variables
. ./environment.setup

VERSION=3071100
########################################################################
##							Sqlite3	
########################################################################

rm -Rf $PREFIX/sqlite-$VERSION
echo "#### Downloading Sqlite library ####"
wget http://www.sqlite.org/sqlite-autoconf-$VERSION.tar.gz
tar -xzf sqlite-autoconf-$VERSION.tar.gz
cd sqlite-autoconf-$VERSION/


export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/gcc-latest/lib64:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/cloog-gcc-latest/lib:$LD_LIBRARY_PATH

echo "#### Building Sqlite library ####"
./configure CC=$CC CXX=$CXX CFLAGS="-O3" CXXFLAGS="-O3" --prefix=$PREFIX/sqlite-$VERSION
make -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Sqlite library ####"
make install

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

rm $PREFIX/sqlite-latest
ln -s $PREFIX/sqlite-$VERSION $PREFIX/sqlite-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf sqlite-*
exit 0
