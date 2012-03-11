# setup environment variables
. environment.setup

VERSION=1.9.3-p125
########################################################################
##								Ruby	
########################################################################
rm -Rf $PREFIX/ruby-$VERSION
echo "#### Downloading Ruby ####"
wget ftp://ftp.ruby-lang.org/pub/ruby/1.9/ruby-$VERSION.tar.gz
tar -xzf ruby-$VERSION.tar.gz
cd ruby-$VERSION

echo "#### Building Ruby ####"
./configure --prefix=$PREFIX/ruby-$VERSION
make -j $SLOTS

echo "#### Installing Ruby ####"
make install 

rm $PREFIX/ruby-latest
ln -s $PREFIX/ruby-$VERSION $PREFIX/ruby-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R ruby-$VERSION*

