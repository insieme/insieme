export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:$PREFIX/gmp-latest/lib:$LD_LIBRARY_PATH
export PATH=$PREFIX/ruby-latest/bin/:$PREFIX/python-latest/bin:$PREFIX/papi-latest/bin:$PATH
CXX=$PREFIX/gcc-latest/bin/g++ LLVM_HOME=$PREFIX/llvm-latest/ GTEST_ROOT=$PREFIX/cmake-latest BOOST_ROOT=$PREFIX/boost-latest INSIEME_LIBS_HOME=$PREFIX $PREFIX/cmake-latest/bin/cmake ~/workspace/insieme2/ -DUSE_XML=OFF -DCMAKE_BUILD_TYPE=Release
