# setup environment variables
. ../environment.setup

VERSION=1.47.0
VERSION_FILENAME=1_47_0
BOOST_LIBS=filesystem,program_options,random,system

########################################################################
##		                Boost
########################################################################
rm -Rf $PREFIX/boost-$VERSION
echo "#### Downloading Boost library ####"
wget http://downloads.sourceforge.net/project/boost/boost/$VERSION/boost_$VERSION_FILENAME.tar.bz2
tar -xf boost_$VERSION_FILENAME.tar.bz2
cd boost_$VERSION_FILENAME

export PATH=$PREFIX/gcc-latest/bin:$PATH

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

