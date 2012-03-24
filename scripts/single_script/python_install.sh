# setup environment variables
. ../environment.setup

VERSION=3.2.2
CFLAGS="-mtune=native -O3 -fgraphite-identity"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Python	
########################################################################
rm -Rf $PREFIX/python-$VERSION
echo "#### Downloading Python ####"
wget http://www.python.org/ftp/python/$VERSION/Python-$VERSION.tgz
tar -xzf Python-$VERSION.tgz
cd Python-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Python ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/python-$VERSION
make -j $SLOTS

echo "#### Installing Python ####"
make install 

rm $PREFIX/python-latest
ln -s $PREFIX/python-$VERSION $PREFIX/python-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R Python-$VERSION*

