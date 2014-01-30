# setup environment variables
. ./environment.setup

VERSION=0.35

OPT_FLAGS="-mtune=native -O3"

########################################################################
##							BARVINOK	
########################################################################

rm -Rf $PREFIX/barvinok-$VERSION
echo "#### Downloading Barvinok library ####"
git clone --branch barvinok-$VERSION git://repo.or.cz/barvinok.git barvinok-$VERSION
cd barvinok-$VERSION
git submodule init
git submodule update
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
