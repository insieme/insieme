# setup environment variables
. ./environment.setup

VERSION=5.22.5
########################################################################
##							ISL
########################################################################

rm -Rf $PREFIX/isl-$VERSION
echo "#### Downloading isl library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/polylib-$VERSION.tar.gz
tar -xf polylib-$VERSION.tar.gz
cd polylib-$VERSION

echo "#### Building isl library ####"
./configure --prefix=$PREFIX/polylib-$VERSION --with-libgmp=$PREFIX/gmp-latest
make -j $SLOTS

echo "#### Installing isl library ####"
make install

rm $PREFIX/polylib-latest
ln -s $PREFIX/polylib-$VERSION $PREFIX/polylib-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf polylib-$VERSION*

