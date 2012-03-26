# setup environment variables
. ../environment.setup

VERSION=0.12
########################################################################
##						PPL	
########################################################################
CFLAGS="-mtune=native -O3 -I$PREFIX/gmp-latest/include"
CXXFLAGS=$CFLAGS

rm -Rf $PREFIX/ppl-$VERSION
echo "#### Downloading ppl library ####"
wget http://bugseng.com/products/ppl/download/ftp/releases/$VERSION/ppl-$VERSION.tar.bz2
tar -xf ppl-$VERSION.tar.bz2
cd ppl-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH

echo "#### Building ppl library ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS LDFLAGS="-L$PREFIX/gmp-latest/lib -mtune=native -O3" CXXFLAGS=$CXXFLAGS ./configure --prefix=$PREFIX/ppl-$VERSION --enable-optimization --with-gmp-prefix=$PREFIX/gmp-latest

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing ppl library ####"
make install

rm $PREFIX/ppl-latest
ln -s $PREFIX/ppl-$VERSION $PREFIX/ppl-latest

echo "#### Cleaning up environment ####"
cd ..
#rm -Rf ppl-$VERSION*

exit 0
