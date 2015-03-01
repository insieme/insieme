# setup environment variables
. ./environment.setup

###########################################################################
#  instalation notes:  
#  ==================
#
#   make sure which llvm llvm-latest is pointing to
#   libLLVM-3.2.so may not exist after compilation    
#             create an alias to libLLVM-3.2svn.so in $PREFIX/llvm-3.2/libs
#   In order to compile clang 3.4, python 2 is needed. If not installed on
#   your machine, please use the python2 install script.
###########################################################################

VERSION=3.4

if [ -d $PREFIX/llvm-$VERSION ]; then
  echo "LLVM version $VERSION already installed"
  exit 0
fi

CURRENT=`pwd`

# download llvm 
echo "*****************************************"
echo "* Downloading current LLVM distribution *"
echo "*****************************************"
wget -nc http://llvm.org/releases/$VERSION/llvm-$VERSION.src.tar.gz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

tar -xf llvm-$VERSION.src.tar.gz
# change dire into tools

echo "******************************************"
echo "* Downloading current CLANG distribution *"
echo "******************************************"

wget -nc http://llvm.org/releases/$VERSION/clang-$VERSION.src.tar.gz 

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

cd llvm-$VERSION/tools

tar -xf ../../clang-$VERSION.src.tar.gz

mv clang-$VERSION clang
cd $CURRENT

#echo "******************************************"
#echo "* Downloading compiler RUNTIME support   *"
#echo "******************************************"
#
#cd llvm-$VERSION.src/projects
#wget -nc http://llvm.org/releases/$VERSION/compiler-rt-$VERSION.src.tar.gz
#
#RET=$?
#if [ $RET -ne 0 ]; then
#	exit $RET
#fi
#
#tar -xf compiler-rt-$VERSION.src.tar.gz
#mv compiler-rt-$VERSION.src compiler-rt
#rm -f compiler-rt-$VERSION.src.tar.gz
#cd $CURRENT

echo "***********************************"
echo "* Applying insieme patch to CLANG *"
echo "***********************************"
cd llvm-$VERSION
patch -p1  < $CURRENT/patches/insieme-clang-$VERSION.patch

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "*******************"
echo "* Compiling CLANG *"
echo "*******************"

export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/gmp-latest/lib:$PREFIX/mpfr-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/mpc-latest/lib/:$LD_LIBRARY_PATH 

CFLAGS="-O3 -std=c++0x"
CC=$CC CXX=$CXX CFLAGS=$CFLAGS CXXFLAGS=$CFLAGS LDFLAGS="-mtune=native -O3" \
	$CURRENT/llvm-$VERSION/configure --prefix=$PREFIX/llvm-$VERSION --enable-shared=yes\
  	 --enable-assert=yes --enable-debug-runtime=no --enable-debug-symbols=no --enable-optimized=yes
# --enable-doxygen=yes

make REQUIRES_RTTI=1 clang-only -j$SLOTS

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	echo " compilation failed "
	exit $RET
fi

#echo "*******************************************"
#echo "* Removing old compilation and installing "
#echo "*******************************************"
#rm -R $PREFIX/llvm-$VERSION
make clang-only install

cd ../
echo "****************************************"
echo "* Removing LLVM installation directory *"
echo "****************************************"
rm -R llvm-$VERSION
rm -f llvm-$VERSION.src.tar.gz
rm -f clang-$VERSION.src.tar.gz


#echo "****************************************************************"
#echo "* Patching stdarg.h to make CLANG work with linux libc (maybe) *"
#echo "****************************************************************"
#patch -d $PREFIX/llvm-$VERSION/lib/clang/$VERSION/include < ./patches/stdarg.patch

rm -f $PREFIX/llvm-latest
ln -s $PREFIX/llvm-$VERSION $PREFIX/llvm-latest

exit 0
