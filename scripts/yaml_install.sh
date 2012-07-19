# setup environment variables
. ./environment.setup

VERSION=0.1.4
CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								Yaml
########################################################################
rm -Rf $PREFIX/libyaml-$VERSION
echo "#### Downloading Yaml ####"
wget http://pyyaml.org/download/libyaml/yaml-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf yaml-$VERSION.tar.gz
cd yaml-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Yaml ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/yaml-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Yaml ####"
make install 

rm -f $PREFIX/yaml-latest
ln -s $PREFIX/yaml-$VERSION $PREFIX/yaml-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R yaml-$VERSION*

exit 0
