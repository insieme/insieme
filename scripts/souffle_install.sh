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
	ed configure.ac <<-'EOF'
		/JAVA
		.,.+2d
		/prof
		d
		/prof
		d
		w
		q
	EOF
	ed Makefile.am <<-EOF
		/prof
		s/ prof//
		w
		q
	EOF
}

if [ -d $PREFIX/souffle-$VERSION ]; then
	echo "Soufflé version $VERSION already installed"
	exit 0
fi

rm -rf $PREFIX/souffle-$VERSION
echo "#### Downloading Soufflé ####"
wget -nc http://www.dps.uibk.ac.at/~csaf7445/ext_libs/souffle-$VERSION.zip

unzip -d souffle-$VERSION souffle-$VERSION.zip
cd souffle-$VERSION/souffle-master

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH

echo "#### Building Soufflé ####"
strip_java_from_souffle
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
