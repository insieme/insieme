# setup environment variables
. environment.setup

VERSION=1.46.1
VERSION_FILENAME=1_46_1
BOOST_LIBS=filesystem,program_options,random,system

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

if [ -f ./b2 ]; then
	# newer boost versions
	./b2 install -j$SLOTS
else
	# older versions of boost (including 1.46.1)	
	./bjam install -j$SLOTS
fi

rm $PREFIX/boost-latest
ln -s $PREFIX/boost-$VERSION $PREFIX/boost-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R boost_$VERSION_FILENAME*




