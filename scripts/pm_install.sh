# setup environment variables
. ./environment.setup

########################################################################
##                                 PM
########################################################################

REPOSITORY=svn+ssh://svn.dps.uibk.ac.at/var/svn/users/eiter/PMLibrary
REVISION=$(svn info $REPOSITORY |grep Revision: |cut -c11-)

echo "#### Removing old build folder ####"

echo $REVISION

mkdir $PREFIX/pm-build

cd $PREFIX/pm-build

echo "#### Checking out PMLibrary ####"
svn co $REPOSITORY .

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Building PMLibrary ####"
make -j $SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing PMLibrary ####"

cd ..

rm -Rf $PREFIX/pm-r*
mkdir $PREFIX/pm-r$REVISION

cp $PREFIX/pm-build/libPM.so $PREFIX/pm-r$REVISION

rm $PREFIX/pm-latest
ln -s $PREFIX/pm-r$REVISION $PREFIX/pm-latest

echo "#### Cleaning up environment ####"
rm -Rf $PREFIX/pm-build

exit 0

