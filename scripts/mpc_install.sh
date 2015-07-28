# setup environment variables
. ./environment.setup

########################################################################
##								MPC
########################################################################

VERSION=0.9
PACKAGE=mpc-$VERSION
FILE=$PACKAGE.tar.gz

if [ -d $PREFIX/mpc-$VERSION ]; then
  echo "MPC version $VERSION already installed"
  exit 0
fi

echo "#### Downloading MPC library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/$FILE

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

# Remove any previous installation dir
rm -Rf $PREFIX/$PACKAGE

tar -xzf $FILE

cd $PACKAGE

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$LD_LIBRARY_PATH

echo "#### Building MPC library ####"
CFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/mpc-$VERSION --with-gmp=$PREFIX/gmp-latest --with-mpfr=$PREFIX/mpfr-latest
make -j $SLOTS
make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing MPC library ####"
make install

rm $PREFIX/mpc-latest
ln -s $PREFIX/$PACKAGE $PREFIX/mpc-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $PACKAGE*

exit 0
