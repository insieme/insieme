PREFIX = /home/motonacciu/software/llvm28

wget http://dps.uibk.ac.at/~spellegrini/llvm-2.8.tar.bz2
tar -xf llvm-2.8.tar.bz2
cd llvm-2.8/

patch -p1  < ../insieme-2.8.patch

./configure --prefix=$PREFIX --enable-shared=yes --enable-assert=yes --enable-debug-runtime=yes --enable-debug-symbols=yes --enable-optimized=no
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j2
make clang-only install

python llvm_alignof.py $PREFIX/include

