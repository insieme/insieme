# setup environment variables
. ../environment.setup

VERSION=0.16.1
########################################################################
##							CLOOG	
########################################################################

echo "#### Downloading Cloog library ####"
wget http://www.bastoul.net/cloog/pages/download/count.php3?url=./cloog-$VERSION.tar.gz -O cloog-$VERSION.tar.gz
tar -xf cloog-$VERSION.tar.gz
cd cloog-$VERSION

rm -Rf $PREFIX/cloog-$VERSION
echo "#### Building Cloog library ####"
CFLAGS="-m64 -O3" LDFLAGS="-m64 -O3" ./configure --prefix=$PREFIX/cloog-$VERSION --with-gmp=system --with-gmp-prefix=$PREFIX/gmp-latest 
make -j $SLOTS

echo "#### Installing Cloog library ####"
make install

rm $PREFIX/cloog-gcc-latest
ln -s $PREFIX/cloog-$VERSION $PREFIX/cloog-gcc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf cloog-$VERSION*

