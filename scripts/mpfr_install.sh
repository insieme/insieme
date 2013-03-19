# setup environment variables
. ./environment.setup

########################################################################
##								MPFR
########################################################################

VERSION=3.1.1
PACKAGE=mpfr-$VERSION
FILE=mpfr-$VERSION.tar.bz2

echo "#### Downloading MPFR library ####"
#wget -nc http://www.mpfr.org/mpfr-current/$FILE
wget -nc http://www.mpfr.org/mpfr-3.1.1/mpfr-3.1.1.tar.bz2

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

rm -Rf $PACKAGE
tar -xf $FILE

cd $PACKAGE

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH

echo "#### Building MPFR library ####"
CFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/mpfr-$VERSION --with-gmp=$PREFIX/gmp-latest 
make -j $SLOTS
make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing MPFR library ####"
make install 

rm $PREFIX/mpfr-latest
ln -s $PREFIX/$PACKAGE $PREFIX/mpfr-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $PACKAGE*

exit 0

