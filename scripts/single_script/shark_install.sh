# setup environment variables
. ../environment.setup

VERSION=2.3.4

########################################################################
##                                                      SHARK 
########################################################################
rm -Rf $PREFIX/shark-$VERSION
echo "#### Downloading SHARK library ####"
wget http://sourceforge.net/projects/shark-project/files/Shark%20Core/Shark%20$VERSION/shark-$VERSION.zip/download -O shark-$VERSION.zip


unzip shark-$VERSION.zip
cd Shark 

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building SHARK library ####"
mkdir build
cd build
CC=$CC CXX=$CXX XFLAGS="-mtune=native -O3" $PREFIX/cmake-latest/bin/cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX/shark-$VERSION
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ] then
	exit $RET
fi

echo "#### Installing SHARK library ####"
make install 

rm -f $PREFIX/shark-latest
ln -s $PREFIX/shark-$VERSION $PREFIX/shark-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -R Shark
rm -R shark-$VERSION* 

exit 0
