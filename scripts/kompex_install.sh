# setup environment variables
. ./environment.setup

VERSION=1.7.9

########################################################################
##                                                      KOMPEX 
########################################################################

if [ -d $PREFIX/kompex-$VERSION ]; then
  echo "KOMPEX version $VERSION already installed"
  exit 0
fi

echo "#### Downloading KOMPEX library ####"
wget -nc http://www.insieme-compiler.org/ext_libs/KompexSQLiteWrapper-Source_$VERSION.tar.gz

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xzf KompexSQLiteWrapper-Source_$VERSION.tar.gz
cd KompexSQLiteWrapper-Source_$VERSION/Kompex\ SQLite\ Wrapper

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

echo "#### Building KOMPEX library ####"
make CXX="$CXX -fPIC -O3" CC="$CC -fPIC -O3" -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

cd ..
echo "#### Installing KOMPEX library ####"

rm -Rf $PREFIX/kompex-$VERSION
rm $PREFIX/kompex-latest
mkdir $PREFIX/kompex-$VERSION
cp -R ./lib $PREFIX/kompex-$VERSION
cp -R ./inc $PREFIX/kompex-$VERSION

ln -s $PREFIX/kompex-$VERSION/lib/debug/KompexSQLiteWrapper_Static_d.a $PREFIX/kompex-$VERSION/lib/libKompexSQLiteWrapper_Static_d.a
ln -s $PREFIX/kompex-$VERSION $PREFIX/kompex-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R KompexSQLiteWrapper-Source_$VERSION*

exit 0
