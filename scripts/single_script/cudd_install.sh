# setup environment variables
. ../environment.setup

VERSION=2.4.2

########################################################################
##								CUDD 
########################################################################
rm -Rf $PREFIX/cudd-$VERSION
echo "#### Downloading CUDD library ####"
wget ftp://vlsi.colorado.edu/pub/cudd-$VERSION.tar.gz
tar -xzf cudd-$VERSION.tar.gz
cd cudd-$VERSION

echo "#### Patching CUDD library ####"
patch -p0 < ../cudd_inline_fix.patch

echo "#### Building CUDD library ####"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

make CC=$CC CXX=$CXX XCFLAGS="-fgraphite-identity -mtune=native -DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS
make testobj CC=$CC CXX=$CXX XCFLAGS="-fgraphite-identity -mtune=native -DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS

# create lib directory
mkdir lib
rm -f ./obj/testobj.o
rm -rf nanotrav

# merge all libraries
find -name *.o -exec ar -r ./lib/libcudd.a \{\} \;

# move result to 
echo "#### Installing CUDD library ####"
cd ..
mv cudd-$VERSION $PREFIX/cudd-$VERSION

rm -f $PREFIX/cudd-latest
ln -s $PREFIX/cudd-$VERSION $PREFIX/cudd-latest

echo "#### Cleaning up environment ####"
# rm -R cudd-$VERSION.tar.gz




