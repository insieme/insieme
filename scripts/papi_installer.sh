PREFIX=/home/philipp/insieme-libs
SLOTS=8

VERSION=papi-4.2.0

##########################################################################
## preliminary prep
##########################################################################

rm $PREFIX/papi-latest
rm -Rf $PREFIX/$VERSION
echo "#### Downloading " $VERSION
wget http://icl.cs.utk.edu/projects/papi/downloads/$VERSION.tar.gz

tar -xf $VERSION.tar.gz

cd $VERSION/src


echo "#### Building " $VERSION

./configure --prefix=$PREFIX/$VERSION

make ICFLAGS=-O3 XCFLAGS="-mtune=native" -j$SLOTS

echo "#### Installing " $VERSION

mkdir $PREFIX/$VERSION

make install

ln -s $PREFIX/$VERSION $PREFIX/papi-latest

echo "#### Cleaning up"

cd ../..

rm -R $VERSION
rm $VERSION.tar.gz

