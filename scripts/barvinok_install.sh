# setup environment variables
. ./environment.setup

VERSION=0.35

OPT_FLAGS="-mtune=native -O3"

########################################################################
##							BARVINOK	
########################################################################

if [ -d $PREFIX/barvinok-$VERSION ]; then
  echo "BARVINOK version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/barvinok-$VERSION
echo "#### Downloading Barvinok library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/barvinok-${VERSION}.tar.gz

tar -xzf barvinok-${VERSION}.tar.gz
rm barvinok-${VERSION}.tar.gz

cd barvinok-$VERSION
./autogen.sh

#export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/ntl-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH 
export LD_LIBRARY_PATH=$PREFIX/ntl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building Barvinok library ####"
CC=$CC CXX=$CXX CFLAGS=$OPT_FLAGS CXXFLAGS=$OPT_FLAGS \
   ./configure --prefix=$PREFIX/barvinok-$VERSION \
   --with-libgmp=$PREFIX/gmp-latest \
   --with-ntl=$PREFIX/ntl-latest \
   --with-isl=system \
   --with-isl-prefix=$PREFIX/isl-latest \
   --with-cloog=no \
   --enable-shared-barvinok

make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Barvinok library ####"
make install

rm $PREFIX/barvinok-latest
ln -s $PREFIX/barvinok-$VERSION $PREFIX/barvinok-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf barvinok-$VERSION*

exit 0
