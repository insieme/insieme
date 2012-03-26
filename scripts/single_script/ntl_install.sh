# setup environment variables
. ../environment.setup

VERSION=5.5.2
########################################################################
##							NTL	
########################################################################

rm -Rf $PREFIX/ntl-$VERSION
echo "#### Downloading ntl library ####"
wget http://shoup.net/ntl/ntl-$VERSION.tar.gz
tar -xzf ntl-$VERSION.tar.gz
cd ntl-$VERSION/src

echo $PREFIX
LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

echo "#### Building ntl library ####"
./configure PREFIX=$PREFIX/ntl-$VERSION NTL_GMP_LIP=on SHARED=on GMP_PREFIX=$PREFIX/gmp-latest/
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ] then
	exit $RET
fi

echo "#### Installing ntl library ####"
make PREFIX=$PREFIX/ntl-$VERSION install

rm $PREFIX/ntl-latest
ln -s $PREFIX/ntl-$VERSION $PREFIX/ntl-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -Rf ntl-$VERSION*
exit 0
