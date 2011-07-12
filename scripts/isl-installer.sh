PREFIX=$HOME/software/isl-lib
echo "#### Downloading ISL library ####"
wget http://www.kotnet.org/~skimo/isl/isl-0.06.tar.bz2
tar -xf isl-0.06.tar.bz2
cd isl-0.06

echo "#### Installing ISL library ####"
./configure --prefix=$PREFIX
make -j8
make install 

echo "#### Cleaning up environment ####"
cd ..
rm -R isl-0.06 isl-0.06.tar.bz2
