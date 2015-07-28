# setup environment variables
. ./environment.setup

VERSION=1.9.3-p125
CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Ruby	
########################################################################

if [ -d $PREFIX/ruby-$VERSION ]; then
  echo "Ruby version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/ruby-$VERSION
echo "#### Downloading Ruby ####"
wget -nc http://www.insieme-compiler.org/ext_libs/ruby-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf ruby-$VERSION.tar.gz
cd ruby-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/yaml-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Ruby ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/ruby-$VERSION --enable-shared --disable-install-doc --with-opt-dir=$PREFIX/yaml-latest
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Ruby ####"
make install 

rm -f $PREFIX/ruby-latest
ln -s $PREFIX/ruby-$VERSION $PREFIX/ruby-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R ruby-$VERSION*

exit 0
