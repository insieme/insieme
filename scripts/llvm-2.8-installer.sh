PREFIX=/home/motonacciu/software/llvm-2.8

# export LD_LIBRARY_PATH=/software/lib-gcc450:/software/gcc450/lib64
export CXX="/home/motonacciu/software/distcc311/bin/distcc g++"
export CC="/home/motonacciu/software/distcc311/bin/distcc gcc"

# download llvm 
wget http://llvm.org/pre-releases/2.8/llvmCore-2.8-rc2.src.tar.gz
tar -xf llvmCore-2.8-rc2.src.tar.gz

# change dire into tools
cd llvmCore-2.8-rc2.src/tools

# download clang
wget http://llvm.org/pre-releases/2.8/clang-2.8-rc2.src.tar.gz
tar -xf clang-2.8-rc2.src.tar.gz
mv clang-2.8-rc2.src clang
cd ../
patch -p1  < ../insieme-2.8.patch

./configure --prefix=$PREFIX --enable-shared=yes --enable-assert=yes --enable-debug-runtime=yes --enable-debug-symbols=yes --enable-optimized=no
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j16
make clang-only install

cd ../
# rm -R llvmCore-2.8-rc2.src
python llvm_alignof.py $PREFIX/include
