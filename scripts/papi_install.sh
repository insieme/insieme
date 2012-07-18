# setup environment variables
. ./environment.setup

VERSION=4.2.1

##########################################################################
## 								Papi
##########################################################################
rm -Rf $PREFIX/papi-$VERSION
echo "#### Downloading Papi Library ####" 
wget -nc http://icl.cs.utk.edu/projects/papi/downloads/papi-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf papi-$VERSION.tar.gz
cd papi-$VERSION/src

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Papi library ####"
CC=$CC CXX=$CXX CFLAGS="-O3 -mtune=native" ./configure --prefix=$PREFIX/papi-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing " papi-$VERSION
make PREFIX=$PREFIX/papi-$VERSION install

rm $PREFIX/papi-latest
ln -s $PREFIX/papi-$VERSION $PREFIX/papi-latest

echo "#### Cleaning up"
cd ../..
rm -R papi-$VERSION*

exit 0

