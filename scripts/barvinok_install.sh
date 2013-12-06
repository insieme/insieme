# setup environment variables
. ./environment.setup

VERSION=0.35

OPT_FLAGS="-mtune=native -O3"

########################################################################
##							BARVINOK	
########################################################################

rm -Rf $PREFIX/barvinok-$VERSION
echo "#### Downloading Barvinok library ####"
#wget -nc ftp://ftp.linux.student.kuleuven.be/pub/people/skimo/barvinok/barvinok-$VERSION.tar.bz2
#wget -nc http://repo.or.cz/w/barvinok.git/snapshot/c8c898a86498fffe11947de849af0bfa1fb42a51.tar.gz
#mv c8c898a86498fffe11947de849af0bfa1fb42a51.tar.gz barvinok-$VERSION.tar.gz
wget -nc http://www.cs.ucla.edu/~pouchet/software/pocc/download/modules/barvinok-0.35.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf barvinok-$VERSION.tar.gz
cd barvinok-$VERSION

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
