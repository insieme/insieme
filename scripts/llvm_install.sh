# setup environment variables
. environment.setup

VERSION=3.0

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget http://llvm.org/releases/$VERSION/llvm-$VERSION.tar.gz 
tar -xf llvm-$VERSION.tar.gz
# change into tools
cd llvm-$VERSION.src/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget http://llvm.org/releases/$VERSION/clang-$VERSION.tar.gz 
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
./configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes --enable-assert=yes --enable-debug-runtime=no \
--enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes
make REQUIRES_RTTI=1 clang-only -j$SLOTS


echo "********************"
echo "* Installing CLANG *"
echo "********************"
make clang-only install

#echo "****************************************************************"
#echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
#echo "****************************************************************"
cd ../
patch -d $PREFIX/llvm-$VERSION/lib/clang/$VERSION/include < stdarg.patch

rm $PREFIX/llvm-latest
ln -s $PREFIX/llvm-$VERSION $PREFIX/llvm-latest

echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -rf llvm-$VERSION.src
rm -rf llvm-$VERSION.tar.gz

