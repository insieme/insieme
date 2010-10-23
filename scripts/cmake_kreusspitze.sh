######
# LIBRARY_PATH

#GCC 4.5.0
export LD_LIBRARY_PATH=/software/lib-gcc450/:/software/gcc450/lib64:/software/gcc450/lib:$LD_LIBRARY_PATH

#LLVM 2.8
export LD_LIBRARY_PATH=/insieme-libs/llvm-2.8/lib/:$LD_LIBRARY_PATH


######
#Path

#Python
export PATH=$PATH:/home/spellegrini/shared/python-2.6.2/bin/



######
#Out of old run_cmake.sh:
export GLOG_HOME=/home/spellegrini/shared/glog031
export BOOST_ROOT=/software/boost1430
export CC=/software/gcc450/bin/gcc
export CTEST_OUTPUT_ON_FAILURE=ON
export CXX=/software/gcc450/bin/g++
export GTEST_ROOT=/home/csaf7445/share/gtest/current
export LLVM_HOME=/home/spellegrini/shared/llvm28/
export XERCES_ROOT_DIR=/home/spellegrini/shared/xerces-c-3.1.1/
/usr/local/cmake-2.8/bin/cmake ..
