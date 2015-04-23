# setup environment variables
. ./environment.setup

########################################################################
##								FLEX
########################################################################
#http://downloads.sourceforge.net/project/flex/flex-2.5.39.tar.gz?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fflex%2Ffiles%2F&ts=1429691732&use_mirror=cznic

VERSION=2.5.39
PACKAGE=flex-$VERSION
FILE=flex-$VERSION.tar.gz

if [ -d $PREFIX/flex-$VERSION ]; then
  echo "flex version $VERSION already installed"
  exit 0
fi

echo "#### Downloading flex library ####"
wget -nc http://downloads.sourceforge.net/project/flex/$FILE

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf $FILE
cd $PACKAGE

echo "#### Building flex library ####"
CFLAGS="-mtune=native -O3" ./configure --prefix=$PREFIX/flex-$VERSION --enable-cxx
make -j $SLOTS
#make check

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

# Remove any previous installation dir
rm -Rf $PREFIX/$PACKAGE

echo "#### Installing flex library ####"
make install 

rm $PREFIX/flex-latest
ln -s $PREFIX/$PACKAGE $PREFIX/flex-latest

echo "#### Cleaning up environment ####"
cd ..
rm -R $PACKAGE*

exit 0

