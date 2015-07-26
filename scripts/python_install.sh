# setup environment variables
. ./environment.setup

VERSION=3.2.2
CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Python	
########################################################################

if [ -d $PREFIX/python-$VERSION ]; then
  echo "Python version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/python-$VERSION
echo "#### Downloading Python ####"
wget -nc http://www.insieme-compiler.org/ext_libs/Python-$VERSION.tgz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf Python-$VERSION.tgz
cd Python-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Python ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/python-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Python ####"
make install 

rm $PREFIX/python-latest
ln -s $PREFIX/python-$VERSION $PREFIX/python-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R Python-$VERSION*

exit 0
