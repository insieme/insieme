
. ../environment.setup

VERSION=3.0

rm -R $PREFIX/llvm-$VERSION

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget -nc http://llvm.org/releases/$VERSION/llvm-$VERSION.tar.gz 
tar -xf llvm-$VERSION.tar.gz
# change dire into tools
cd llvm-$VERSION.src/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget -nc http://llvm.org/releases/$VERSION/clang-$VERSION.tar.gz 
tar -xf clang-$VERSION.tar.gz
mv clang-$VERSION.src clang
rm -f clang-$VERSION.tar.gz
cd ../

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
patch -p1  < ../insieme-$VERSION.patch

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$LD_LIBRARY_PATH 

CFLAGS="-mtune=native -O3 -fgraphite-identity"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CFLAGS LDFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes \
  	 --enable-assert=yes --enable-debug-runtime=no --enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-$VERSION.src
rm -f llvm-$VERSION.tar.gz


#echo "****************************************************************"
#echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
#echo "****************************************************************"
patch -d $PREFIX/llvm-$VERSION/lib/clang/$VERSION/include < stdarg.patch

rm -f $PREFIX/llvm-latest
ln -s $PREFIX/llvm-$VERSION $PREFIX/llvm-latest

exit 0
