# setup environment variables
. environment.setup

VERSION=1.48.0
VERSION_FILENAME=1_48_0
BOOST_LIBS=filesystem,program_options,random

########################################################################
##		                Boost
########################################################################
rm -Rf $PREFIX/boost-$VERSION
echo "#### Downloading Boost library ####"
wget http://downloads.sourceforge.net/project/boost/boost/$VERSION/boost_$VERSION_FILENAME.tar.gz
tar -xzf boost_$VERSION_FILENAME.tar.gz
cd boost_$VERSION_FILENAME


echo "#### Building and installing Boost libraries ####"
mkdir $PREFIX/boost-$VERSION
./bootstrap.sh --prefix=$PREFIX/boost-$VERSION --with-libraries=$BOOST_LIBS
./b2 install

ln -sf $PREFIX/boost-$VERSION $PREFIX/boost-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R boost_$VERSION*




