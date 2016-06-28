# setup environment variables
. ./environment.setup
set -e

VERSION=2.4.5

########################################################################
##                            GNU LIBTOOL
########################################################################

if [ -d $PREFIX/libtool-$VERSION ]; then
	echo "Libtool version $VERSION already installed"
	exit 0
fi

rm -rf $PREFIX/libtool-$VERSION
echo "#### Downloading Libtool ####"
wget -nc http://ftp.gnu.org/gnu/libtool/libtool-${VERSION}.tar.xz
#cp ~/libtool-$VERSION.tar.xz .
tar -xvf libtool-$VERSION.tar.xz

cd libtool-$VERSION

echo "#### Building Libtool ####"
./configure --prefix=$PREFIX/libtool-$VERSION
make -j $SLOTS

echo "#### Installing Libtool ####"
mkdir $PREFIX/libtool-$VERSION
make install

rm -f $PREFIX/libtool-latest
ln -s $PREFIX/libtool-$VERSION $PREFIX/libtool-latest

echo "#### Cleaning up environment ####"
cd ..
rm -rf libtool-$VERSION*

exit 0
