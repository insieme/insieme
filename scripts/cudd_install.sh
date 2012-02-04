
PREFIX=/home/herbert/libs
SLOTS=8

CUDD_VER=cudd-2.4.2

# using default system compiler
CC=${CC:-gcc}
CXX=${CXX:-g++}

########################################################################
##								CUDD 
########################################################################
rm -Rf $PREFIX/$CUDD_VER
echo "#### Downloading CUDD library ####"
wget ftp://vlsi.colorado.edu/pub/$CUDD_VER.tar.gz
tar -xzf $CUDD_VER.tar.gz
cd $CUDD_VER

echo "#### Patching CUDD library ####"
patch -p0 < ../cudd_inline_fix.patch

echo "#### Building CUDD library ####"

make ICFLAGS=-O3 CC=$CC CXX=$CXX XCFLAGS="-mtune=native -DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS
make testobj ICFLAGS=-O3 CC=$CC CXX=$CXX XCFLAGS="-mtune=native -DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS

# create lib directory
mkdir lib
rm ./obj/testobj.o
rm -r nanotrav

# merge all libraries
find -name *.o -exec ar -r ./lib/libcudd.a \{\} \;

# move result to 
echo "#### Installing CUDD library ####"
cd ..
mv $CUDD_VER $PREFIX/$CUDD_VER

rm $PREFIX/cudd-latest
ln -s $PREFIX/$CUDD_VER $PREFIX/cudd-latest

echo "#### Cleaning up environment ####"
rm -R $CUDD_VER.tar.gz




