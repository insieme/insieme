# setup environment variables
. ./environment.setup

VERSION=1.5.3

########################################################################
##							NINJA
########################################################################

if [ -d $PREFIX/ninja-$VERSION ]; then
  echo "NINJA version $VERSION already installed"
  exit 0
fi

rm -Rf $PREFIX/ninja-$VERSION
echo "#### Downloading Ninja ####"
git clone git://github.com/martine/ninja.git ninja-$VERSION
cd ninja-$VERSION
git checkout tags/v$VERSION

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/ntl-latest/lib:$PREFIX/mpc-latest/lib:$LD_LIBRARY_PATH 
export PATH=$PREFIX/python-latest/bin:$PATH

echo "#### Building Ninja ####"
python configure.py --bootstrap

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Installing Ninja ####"
mkdir $PREFIX/ninja-$VERSION
mkdir $PREFIX/ninja-$VERSION/bin
mv ninja $PREFIX/ninja-$VERSION/bin

rm -f $PREFIX/ninja-latest
ln -s $PREFIX/ninja-$VERSION $PREFIX/ninja-latest

echo "#### Cleaning up environment ####"
cd ..
rm -Rf ninja-$VERSION*

exit 0
