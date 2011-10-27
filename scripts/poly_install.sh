
PREFIX=/tmp/
SLOTS=4

ISL_VER=isl-0.07
BARVINOK_VER=barvinok-0.34
NTL_VER=ntl-5.5.2
CLOOG_VER=cloog-0.16.3

########################################################################
##								ISL 
########################################################################
rm -Rf $PREFIX/$ISL_VER
echo "#### Downloading ISL library ####"
wget http://www.kotnet.org/~skimo/isl/$ISL_VER.tar.bz2
tar -xf $ISL_VER.tar.bz2
cd $ISL_VER

echo "#### Installing ISL library ####"
./configure --prefix=$PREFIX/$ISL_VER
make -j $SLOTS
make install 

ln -s $PREFIX/$ISL_VER $PREFIX/isl-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $ISL_VER $ISL_VER.tar.bz2
########################################################################
##							NTL 
########################################################################
rm -Rf $PREFIX/$NTL_VER
echo "#### Downloading NTL library ####"
wget http://shoup.net/ntl/$NTL_VER.tar.gz
tar -xf $NTL_VER.tar.gz
cd $NTL_VER/src

echo "#### Installing NTL library ####"
./configure PREFIX=$PREFIX/$NTL_VER NTL_GMP_LIP=on SHARED=on
make -j $SLOTS
make install

ln -s $PREFIX/$NTL_VER $PREFIX/ntl-latest

echo "#### Cleaning up environment ####"
cd ../..
rm -R $NTL_VER $NTL_VER.tar.gz

########################################################################
##							CLOOG 
########################################################################
rm -Rf $PREFIX/$CLOOG_VER
echo "#### Downloading Cloog library ####"
wget http://www.bastoul.net/cloog/pages/download/count.php3?url=./$CLOOG_VER.tar.gz -O $CLOOG_VER.tar.gz
tar -xf $CLOOG_VER.tar.gz
cd $CLOOG_VER
 
echo "#### Installing cloog library ####"
./configure --prefix=$PREFIX/$CLOOG_VER --with-gmp --with-isl=system --with-isl-prefix=$PREFIX/isl-latest
make -j $SLOTS
make install

ln -s $PREFIX/$CLOOG_VER $PREFIX/cloog-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $CLOOG_VER $CLOOG_VER.tar.gz

########################################################################
##							BARVINOK 
########################################################################
rm -Rf $PREFIX/$BARVINOK_VER
echo "#### Downloading barvinok library ####"
wget http://www.kotnet.org/~skimo/barvinok/$BARVINOK_VER.tar.bz2
tar -xf $BARVINOK_VER.tar.bz2
cd $BARVINOK_VER

echo "#### Installing barvinok library ####"
./configure --prefix=$PREFIX/$BARVINOK_VER --with-ntl=$PREFIX/ntl-latest \
			--with-isl=system --with-isl-prefix=$PREFIX/isl-latest \
			--with-cloog=no --enable-shared-barvinok
make -j $SLOTS
make install

ln -s $PREFIX/$BARVINOK_VER $PREFIX/barvinok-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $BARVINOK_VER $BARVINOK_VER.tar.bz2







