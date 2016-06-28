# setup environment variables
. ./environment.setup
set -e

VERSION=2.68

########################################################################
##                            GNU AUTOCONF
########################################################################

if [ -d $PREFIX/autoconf-$VERSION ]; then
	echo "Autoconf version $VERSION already installed"
	exit 0
fi

rm -rf $PREFIX/autoconf-$VERSION
echo "#### Downloading Autoconf ####"
wget -nc http://ftp.gnu.org/gnu/autoconf/autoconf-${VERSION}.tar.bz2
#cp ~/autoconf-$VERSION.tar.bz2 .
tar -xvf autoconf-$VERSION.tar.bz2

cd autoconf-$VERSION

echo "#### Building Autoconf ####"
./configure --prefix=$PREFIX/autoconf-$VERSION
make -j $SLOTS

echo "#### Installing Autoconf ####"
mkdir $PREFIX/autoconf-$VERSION
make install

rm -f $PREFIX/autoconf-latest
ln -s $PREFIX/autoconf-$VERSION $PREFIX/autoconf-latest

echo "#### Cleaning up environment ####"
cd ..
rm -rf autoconf-$VERSION*

exit 0
