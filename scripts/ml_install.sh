# setup environment variables
. environment.setup

OLD_KOMPEX_VER=KompexSQLiteWrapper-Source_1.7.8
KOMPEX_VER=KompexSQLiteWrapper-Source_1.7.9
SHARK_VER=Shark

########################################################################
##							KOMPEX 
########################################################################
rm -Rf $PREFIX/$OLD_KOMPEX_VER
cd $PREFIX
echo "#### Downloading KOMPEX library ####"
wget http://sqlitewrapper.kompex-online.com/counter/download.php?dl=$KOMPEX_VER.tar.gz --output-document=$KOMPEX_VER.tar.gz
tar -xf $KOMPEX_VER.tar.gz
cd $KOMPEX_VER/Kompex\ SQLite\ Wrapper

echo "#### Installing KOMPEX library ####"
make CXX="g++ -fPIC" CC="gcc -fPIC" -j$SLOTS

ln -s $PREFIX/$KOMPEX_VER/lib/debug/KompexSQLiteWrapper_Static_d.a $PREFIX/$KOMPEX_VER/lib/libKompexSQLiteWrapper_Static_d.a

rm $PREFIX/kompex-latest
ln -s $PREFIX/$KOMPEX_VER $PREFIX/kompex-latest

echo "#### Cleaning up environment ####"
cd ..
#rm -R Kompex\ SQLite\ Wrapper src CHANGELOG.txt Kompex\ SQLite\ Wrapper\ VS*
#rm $PREFIX/$KOMPEX_VER.tar.gz


########################################################################
##							SHARK 
########################################################################
rm -Rf $PREFIX/$SHARK_VER
cd $PREFIX

echo "#### Downloading SHARK library ####"
wget http://sourceforge.net/projects/shark-project/files/latest/download?source=files --output-document=$SHARK_VER.zip
unzip $SHARK_VER.zip
cd $SHARK_VER

# patch shark
patch -p0 < svm.cpp.patch
patch -p0 < svm.h.patch


echo "#### Installing KOMPEX library ####"
./installShark

rm $PREFIX/shark-latest
ln -s $PREFIX/$SHARK_VER $PREFIX/shark-latest

echo "#### Cleaning up environment ####"
rm $PREFIX/$SHARK_VER.zip

