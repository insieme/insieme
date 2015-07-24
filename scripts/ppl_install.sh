# setup environment variables
. ./environment.setup

########################################################################
##						PPL	
########################################################################

VERSION=0.12.1
PACKAGE=ppl-$VERSION
FILE=$PACKAGE.tar.bz2

if [ -d $PREFIX/ppl-$VERSION ]; then
  echo "PPL version $VERSION already installed"
  exit 0
fi

CFLAGS="-mtune=native -O3"
CXXFLAGS=$CFLAGS

echo "#### Downloading ppl library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/$FILE

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

rm -Rf $PACKAGE
tar -xf $FILE

cd $PACKAGE

patch -p0 < ../patches/ppl_numeric_limits.patch

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH

echo "#### Building ppl library ####"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS ./configure --prefix=$PREFIX/ppl-$VERSION --enable-optimization --with-gmp=$PREFIX/gmp-latest

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing ppl library ####"
make install

rm $PREFIX/ppl-latest
ln -s $PREFIX/$PACKAGE $PREFIX/ppl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf $PACKAGE*

exit 0
