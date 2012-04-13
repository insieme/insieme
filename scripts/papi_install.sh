# setup environment variables
. environment.setup

VERSION=4.2.1

##########################################################################
## 								Papi
##########################################################################
rm -Rf $PREFIX/papi-$VERSION
echo "#### Downloading Papi Library ####" 
wget http://icl.cs.utk.edu/projects/papi/downloads/papi-$VERSION.tar.gz
tar -xf papi-$VERSION.tar.gz
cd papi-$VERSION/src

echo "#### Building Papi library ####"
./configure --prefix=$PREFIX/papi-$VERSION
make ICFLAGS=-O3 XCFLAGS="-mtune=native" -j $SLOTS

echo "#### Installing " papi-$VERSION
make install

rm $PREFIX/papi-latest
ln -s $PREFIX/papi-$VERSION $PREFIX/papi-latest

echo "#### Cleaning up"
cd ../..
rm -R papi-$VERSION*

