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

export LD_LIBRARY_PATH=$PREFIX/gmp-latest/lib:$PREFIX/gcc-latest/lib64:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/cloog-gcc-latest/lib:$LD_LIBRARY_PATH

echo "#### Building ntl library ####"
./configure CC=$CC CXX=$CXX CFLAGS="-O3" CXXFLAGS="-O3" PREFIX=$PREFIX/ntl-$VERSION NTL_GMP_LIP=on SHARED=on GMP_PREFIX=$PREFIX/gmp-latest/
make -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing ntl library ####"
make PREFIX=$PREFIX/ntl-$VERSION NTL_GMP_LIP=on SHARED=on install

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

rm $PREFIX/ntl-latest
ln -s $PREFIX/ntl-$VERSION $PREFIX/ntl-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -Rf ntl-$VERSION*
exit 0
