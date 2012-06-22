# setup environment variables
. ./environment.setup

########################################################################
##								MPFR
########################################################################

VERSION=3.1.0
PACKAGE=mpfr-$VERSION
FILE=mpfr-$VERSION.tar.gz

echo "#### Downloading MPFR library ####"
wget -nc http://mpfr.loria.fr/mpfr-current/$FILE

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

rm -Rf $PACKAGE
tar -xzf $FILE

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

