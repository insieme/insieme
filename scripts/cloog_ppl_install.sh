# setup environment variables
. ./environment.setup

########################################################################
##							CLOOG	
########################################################################

VERSION=0.15.11
PACKAGE=cloog-ppl-$VERSION
FILE=$PACKAGE.tar.gz

rm -Rf $PREFIX/cloog-ppl-$VERSION
echo "#### Downloading Cloog library ####"
wget -nc ftp://gcc.gnu.org/pub/gcc/infrastructure/$FILE

rm -Rf $PACKAGE
tar -xf $FILE
cd $PACKAGE

echo "#### Building Cloog library ####"
./configure --prefix=$PREFIX/$PACKAGE --with-gmp=$PREFIX/gmp-latest --enable-shared --with-bits=gmp --with-ppl=$PREFIX/ppl-latest

make -j $SLOTS

echo "#### Installing Cloog library ####"
make install

rm $PREFIX/cloog-ppl-latest
ln -s $PREFIX/$PACKAGE $PREFIX/cloog-ppl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf $PACKAGE*

