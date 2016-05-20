# setup environment variables
. ./environment.setup
set -e

VERSION=2016.05.15

########################################################################
##                       SOUFFLÉ DATALOG ENGINE
########################################################################

patch_souffle()
{
	cp $patchdir/boost.m4 m4
	patch -Np1 < $patchdir/souffle_strip_java.patch
	patch -Np1 < $patchdir/souffle_change_boost_macros.patch
}


if [ -d $PREFIX/souffle-$VERSION ]; then
	echo "Soufflé version $VERSION already installed"
	exit 0
fi


# Set correct PATH and other variables
isl=$PREFIX
autoconf=$isl/autoconf-latest/bin
automake=$isl/automake-latest/bin
libtool=$isl/libtool-latest/bin
flex=$isl/flex-latest/bin
bison=$isl/bison-latest/bin

export BOOST_ROOT=$isl/boost-latest
export PATH=$automake:$autoconf:$libtool:$flex:$bison:$PATH

patchdir="$(pwd)/patches"

echo "#### Downloading Soufflé ####"
wget -nc http://www.dps.uibk.ac.at/~csaf7445/ext_libs/souffle-$VERSION.zip
unzip -o -d souffle-$VERSION souffle-$VERSION.zip

cd souffle-$VERSION/souffle

echo "#### Building Soufflé ####"
patch_souffle
libtoolize
aclocal
autoheader
automake --gnu --add-missing
autoconf || autoconf # Fails the first time
./configure --prefix=$PREFIX/souffle-$VERSION
make -j $SLOTS

echo "#### Installing Soufflé ####"
make install

rm -f $PREFIX/souffle-latest
ln -s $PREFIX/souffle-$VERSION $PREFIX/souffle-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -rf souffle-$VERSION*

exit 0
