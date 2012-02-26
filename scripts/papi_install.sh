# setup environment variables
. environment.setup

VERSION=4.2.0

##########################################################################
## preliminary prep
##########################################################################

rm $PREFIX/papi-latest
rm -Rf $PREFIX/$VERSION
echo "#### Downloading " $VERSION
wget http://icl.cs.utk.edu/projects/papi/downloads/papi-$VERSION.tar.gz

tar -xf papi-$VERSION.tar.gz

cd papi-$VERSION/src


echo "#### Building " papi-$VERSION

./configure --prefix=$PREFIX/papi-$VERSION

make ICFLAGS=-O3 XCFLAGS="-mtune=native" -j$SLOTS

echo "#### Installing " papi-$VERSION

mkdir $PREFIX/papi-$VERSION
make install

ln -sf $PREFIX/papi-$VERSION $PREFIX/papi-latest

echo "#### Cleaning up"

cd ../..

rm -R papi-$VERSION
rm papi-$VERSION.tar.gz

