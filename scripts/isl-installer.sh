<<<<<<< HEAD
PREFIX=/insieme-libs/isl-lib
=======
PREFIX=/insieme/libs/isl
>>>>>>> 34461e3e47da26c0e667806a5e0513a8fff1d8dd
echo "#### Downloading ISL library ####"
wget http://www.kotnet.org/~skimo/isl/isl-0.06.tar.bz2
tar -xf isl-0.06.tar.bz2
cd isl-0.06

echo "#### Installing ISL library ####"
./configure --prefix=$PREFIX
make -j4
make install 

echo "#### Cleaning up environment ####"
cd ..
rm -R isl-0.06 isl-0.06.tar.bz2
