HOME=/home/spellegrini/workspace/insieme2/
SRC_DIR=/home/spellegrini/shared/llvm27-src/
PREFIX_DIR=/home/spellegrini/shared/llvm27/

mkdir llvm_build
cd llvm_build

# download llvm 
wget http://llvm.org/releases/2.7/llvm-2.7.tgz
tar -xf llvm-2.7.tgz

# change dire into tools
cd llvm-2.7/tools
# download clang
wget http://llvm.org/releases/2.7/clang-2.7.tgz
tar -xf clang-2.7.tgz
mv clang-2.7 clang
cd ../../
patch -p1 < ../insieme.patch

# move the dir into the src dir
mv llvm-2.7 $SRC_DIR
# remove the tmp build dir
cd ..
rm -R llvm_build

cd $SRC_DIR
./configure --prefix=$PREFIX_DIR --enable-shared
make clang-only -j4
make install clang-only

# change alignof method defined in llvm in llvm_alignof for compatibility with C++0x
python $HOME/scripts/llvm_alignof.py $PREFIX_DIR/include/
