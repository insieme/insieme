# setup environment variables
. ./environment.setup

VERSION=0.16.1
########################################################################
##							CLOOG	
########################################################################

if [ -d $PREFIX/cloog-gcc-$VERSION ]; then
  echo "CLOOG-GCC version $VERSION already installed"
  exit 0
fi

echo "#### Downloading Cloog library ####"
wget -nc http://www.bastoul.net/cloog/pages/download/count.php3?url=./cloog-$VERSION.tar.gz -O cloog-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf cloog-$VERSION.tar.gz

cd cloog-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH 

rm -Rf $PREFIX/cloog-gcc-$VERSION

echo "#### Building Cloog library ####"
CFLAGS="-mtune=native -O3" ./configure \
		--prefix=$PREFIX/cloog-gcc-$VERSION \
		--with-gmp=system \
		--with-gmp-prefix=$PREFIX/gmp-latest 

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Cloog library ####"
make install

rm $PREFIX/cloog-gcc-latest
ln -s $PREFIX/cloog-gcc-$VERSION $PREFIX/cloog-gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf cloog-$VERSION*

exit 0
