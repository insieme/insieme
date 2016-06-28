# setup environment variables
. ./environment.setup
set -e

VERSION=1.15

########################################################################
##                            GNU AUTOMAKE
########################################################################

if [ -d $PREFIX/automake-$VERSION ]; then
	echo "automake version $VERSION already installed"
	exit 0
fi

export PATH=$NEW_AUTOCONF_PATH:$PATH

rm -rf $PREFIX/automake-$VERSION
echo "#### Downloading automake ####"
wget -nc http://ftp.gnu.org/gnu/automake/automake-${VERSION}.tar.xz
#cp ~/automake-$VERSION.tar.xz .
tar -xvf automake-$VERSION.tar.xz

cd automake-$VERSION

echo "#### Building Automake ####"
./configure --prefix=$PREFIX/automake-$VERSION
make -j $SLOTS

echo "#### Installing Automake ####"
mkdir $PREFIX/automake-$VERSION
make install

rm -f $PREFIX/automake-latest
ln -s $PREFIX/automake-$VERSION $PREFIX/automake-latest

echo "#### Cleaning up environment ####"
cd ..
rm -rf automake-$VERSION*

exit 0
