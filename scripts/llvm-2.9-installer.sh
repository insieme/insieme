PREFIX=/insieme-libs/llvm29
SLOTS=16

export LD_LIBRARY_PATH=/insieme-libs/gcc452/lib64
export CXX="/insieme-libs/gcc452/bin/g++"
export CC="/insieme-libs/gcc452/bin/gcc"

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget http://llvm.org/releases/2.9/llvm-2.9.tgz 
tar -xf llvm-2.9.tgz
# change dire into tools
cd llvm-2.9/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget http://llvm.org/releases/2.9/clang-2.9.tgz 
tar -xf clang-2.9.tgz
mv clang-2.9 clang
rm -f clang-2.9.tgz
cd ../

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
patch -p1  < ../insieme-2.9.patch

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"
./configure --prefix=$PREFIX --enable-shared=yes --enable-assert=no --enable-debug-runtime=no \
--enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS
make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-2.9

echo "****************************************************************"
echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
echo "****************************************************************"
patch -d $PREFIX/lib/clang/2.9/include < stdarg.patch
