PREFIX=/insieme-libs/llvm-2.9
SLOTS=16

export LD_LIBRARY_PATH=/insieme-libs/gcc452/lib64
export CXX="/insieme-libs/gcc452/bin/g++"
export CC="/insieme-libs/gcc452/bin/gcc"

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget http://llvm.org/pre-releases/2.9/llvm-2.9rc1.src.tar.gz
gzip -df llvm-2.9rc1.src.tar.gz
tar -xf llvm-2.9rc1.src.tar
# change dire into tools
cd llvm-2.9rc1/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget http://llvm.org/pre-releases/2.9/clang-2.9rc1.src.tar.gz
gzip -df clang-2.9rc1.src.tar.gz 
tar -xf clang-2.9rc1.src.tar
mv clang-2.9rc1 clang
rm -f clang-2.9rc1.src.tar
cd ../

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
patch -p1  < ../insieme-2.9.patch

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"
./configure --prefix=$PREFIX --enable-shared=yes --enable-assert=yes --enable-debug-runtime=yes --enable-debug-symbols=yes --enable-optimized=no
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS
make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-2.9rc1

echo "****************************************************************"
echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
echo "****************************************************************"
patch -d $PREFIX/lib/clang/2.9/include < stdarg.patch
