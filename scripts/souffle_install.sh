# setup environment variables
. ./environment.setup
set -e

VERSION=2016.04.20

########################################################################
##                       SOUFFLÉ DATALOG ENGINE
########################################################################

strip_java_from_souffle()
{
	rm configure Makefile.in
	echo "$1" | patch -Np1
}

if [ -d $PREFIX/souffle-$VERSION ]; then
	echo "Soufflé version $VERSION already installed"
	exit 0
fi

rm -rf $PREFIX/souffle-$VERSION
echo "#### Downloading Soufflé ####"
wget -nc http://www.dps.uibk.ac.at/~csaf7445/ext_libs/souffle-$VERSION.zip
unzip -d souffle-$VERSION souffle-$VERSION.zip

java_patch="$(cat patches/souffle_strip_java.patch)"
cd souffle-$VERSION/souffle-master

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH

echo "#### Building Soufflé ####"
strip_java_from_souffle "$java_patch"
sh bootstrap
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
