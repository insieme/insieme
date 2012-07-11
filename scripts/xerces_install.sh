# setup environment variables
. ./environment.setup

VERSION=3.1.1

########################################################################
##		xerces
########################################################################
rm -Rf $PREFIX/xerces-$VERSION
echo "#### Downloading Xerces library ####"
wget http://mirror.sti2.at/apache//xerces/c/3/sources/xerces-c-$VERSION.tar.gz
tar -xzf xerces-c-$VERSION.tar.gz
cd xerces-c-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Xerces library ####"
CC=$CC CXX=$CXX XFLAGS="-mtune=native -O3 -fgraphite-identity" LDFLAGS="-O3 -mtune=native" ./configure --prefix=$PREFIX/xerces-$VERSION
cd src
make -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Xerces library ####"
make install
cd ..

ln -sf $PREFIX/xerces-$VERSION $PREFIX/xerces-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R xerces-c-$VERSION*

exit 0

