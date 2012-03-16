PREFIX=~/insieme-deps/llvm30
SLOTS=3

#export LD_LIBRARY_PATH=/insieme-libs/gcc452/lib64
#export CXX="/insieme-libs/gcc452/bin/g++"
#export CC="/insieme-libs/gcc452/bin/gcc"

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget http://llvm.org/releases/3.0/llvm-3.0.tar.gz 
tar -xf llvm-3.0.tar.gz
# change dire into tools
cd llvm-3.0.src/tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"
# download clang
wget http://llvm.org/releases/3.0/clang-3.0.tar.gz 
tar -xf clang-3.0.tar.gz
mv clang-3.0.src clang
rm -f clang-3.0.tar.gz
cd ../

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
patch -p1  < ../insieme-3.0.patch

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"
./configure --prefix=$PREFIX --enable-shared=yes --enable-assert=yes --enable-debug-runtime=no \
--enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS
make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-3.0.src

#echo "****************************************************************"
#echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
#echo "****************************************************************"
patch -d $PREFIX/lib/clang/3.0/include < stdarg.patch
