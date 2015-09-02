# setup environment variables
. ./environment.setup

VER=1.10
VERSION=1.10.1
CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS
LDFLAGS="-mtune=native -O3"

########################################################################
##								HWLOC
########################################################################

if [ -d $PREFIX/hwloc-$VERSION ]; then
  echo "HWLOC version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/hwloc-$VERSION
echo "#### Downloading HWLOC ####"
wget -nc http://www.insieme-compiler.org/ext_libs/hwloc-$VERSION.tar.gz

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf hwloc-$VERSION.tar.gz
cd hwloc-$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH 

echo "#### Building Hwloc ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CXXFLAGS LDFLAGS=$LDFLAGS ./configure --prefix=$PREFIX/hwloc-$VERSION --disable-libxml2
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Hwloc ####"
make install 

rm $PREFIX/hwloc-latest
ln -s $PREFIX/hwloc-$VERSION $PREFIX/hwloc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R hwloc-$VERSION*

exit 0
