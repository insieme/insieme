# setup environment variables
. ../environment.setup

VERSION=0.11.2
########################################################################
##						PPL	
########################################################################

rm -Rf $PREFIX/ppl-$VERSION
echo "#### Downloading ppl library ####"
wget http://bugseng.com/products/ppl/download/ftp/releases/$VERSION/ppl-$VERSION.tar.bz2
tar -xf ppl-$VERSION.tar.bz2
cd ppl-$VERSION

echo "#### Building ppl library ####"
CFLAGS="-m64 -O3" LDFLAGS="-m64 -O3" CXXFLAGS="-m64 -O3" ./configure --prefix=$PREFIX/ppl-$VERSION --enable-optimization --with-gmp-prefix=$PREFIX/gmp-latest

make -j $SLOTS

echo "#### Installing ppl library ####"
make install

rm $PREFIX/ppl-latest
ln -s $PREFIX/ppl-$VERSION $PREFIX/ppl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf ppl-$VERSION*

