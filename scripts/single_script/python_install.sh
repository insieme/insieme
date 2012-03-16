# setup environment variables
. ../environment.setup

VERSION=2.7.2
########################################################################
##								Python	
########################################################################
rm -Rf $PREFIX/python-$VERSION
echo "#### Downloading Python ####"
wget http://www.python.org/ftp/python/$VERSION/Python-$VERSION.tgz
tar -xzf Python-$VERSION.tgz
cd Python-$VERSION

echo "#### Building Python ####"
./configure --prefix=$PREFIX/python-$VERSION
make -j $SLOTS

echo "#### Installing Python ####"
make install 

rm $PREFIX/python-latest
ln -s $PREFIX/python-$VERSION $PREFIX/python-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R Python-$VERSION*

