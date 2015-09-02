# setup environment variables
. ./environment.setup

VERSION=2.4.2

########################################################################
##								CUDD 
########################################################################

if [ -d $PREFIX/cudd-$VERSION ]; then
  echo "CUDD version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/cudd-$VERSION
echo "#### Downloading CUDD library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/cudd-$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf cudd-$VERSION.tar.gz
cd cudd-$VERSION

echo "#### Patching CUDD library ####"
patch -p0 < ../patches/cudd_inline_fix.patch

echo "#### Building CUDD library ####"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$LD_LIBRARY_PATH 

make CC=$CC CPP=$CXX ICFLAGS="-O3" XCFLAGS="-DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS
make testobj CC=$CC CPP=$CXX ICFLAGS="-O3" XCFLAGS="-DHAVE_IEEE_754 -DBSD -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -fPIC" -j $SLOTS

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

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
rm -R cudd-$VERSION.tar.gz

exit 0



