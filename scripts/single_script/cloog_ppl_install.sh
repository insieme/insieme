# setup environment variables
. ../environment.setup

VERSION=0.15.11
########################################################################
##							CLOOG	
########################################################################

rm -Rf $PREFIX/cloog-ppl-$VERSION
echo "#### Downloading Cloog library ####"
wget ftp://gcc.gnu.org/pub/gcc/infrastructure/cloog-ppl-$VERSION.tar.gz
tar -xf cloog-ppl-$VERSION.tar.gz
cd cloog-ppl-$VERSION

echo "#### Building Cloog library ####"
./configure --prefix=$PREFIX/cloog-ppl-$VERSION --with-gmp=$PREFIX/gmp-latest --enable-shared --with-bits=gmp --with-ppl=$PREFIX/ppl-latest
make -j $SLOTS

echo "#### Installing Cloog library ####"
make install

rm $PREFIX/cloog-ppl-latest
ln -s $PREFIX/cloog-ppl-$VERSION $PREFIX/cloog-ppl-latest

echo "#### Cleaning up environment ####"
cd ..
#rm -Rf cloog-ppl-$VERSION*

