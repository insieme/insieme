# setup environment variables
. ../environment.setup

VERSION=0.16.1
########################################################################
##							CLOOG	
########################################################################

echo "#### Downloading Cloog library ####"
wget -nc http://www.bastoul.net/cloog/pages/download/count.php3?url=./cloog-$VERSION.tar.gz -O cloog-$VERSION.tar.gz
tar -xf cloog-$VERSION.tar.gz

cd cloog-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH 

rm -Rf $PREFIX/cloog-$VERSION

echo "#### Building Cloog library ####"
CFLAGS="-mtune=native -O3" ./configure \
		--prefix=$PREFIX/cloog-$VERSION \
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
ln -s $PREFIX/cloog-$VERSION $PREFIX/cloog-gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf cloog-$VERSION*

exit 0
