# setup environment variables
. ./environment.setup

########################################################################
##								BISON
########################################################################
#http://ftp.gnu.org/gnu/bison/bison-3.0.4.tar.gz

VERSION=3.0.4
PACKAGE=bison-$VERSION
FILE=bison-$VERSION.tar.gz

if [ -d $PREFIX/bison-$VERSION ]; then
  echo "bison version $VERSION already installed"
  exit 0
fi

echo "#### Downloading BISON library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/bison-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar xf $FILE
cd $PACKAGE

echo "#### Building BISON library ####"
CFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/bison-$VERSION 
make -j $SLOTS
#make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

# Remove any previous installation dir
rm -Rf $PREFIX/$PACKAGE

echo "#### Installing BISON library ####"
make install 

rm $PREFIX/bison-latest
ln -s $PREFIX/$PACKAGE $PREFIX/bison-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $PACKAGE*

exit 0

