# optional: set up the CC and CXX compiler to be used
#export CXX=g++
#export CC=gcc

# run cmake to create the project
../third_party/cmake/bin/cmake \
	-DOPENCL_ROOT=/path/to/opencl \
	-DSOUFFLE_ROOT=../third_party/souffle \
	-DSTACK_ROOT=../third_party/stack \
	-DCMAKE_BUILD_TYPE=Debug \
	-DBUILD_DOCS=ON  \
	-DANALYSIS_DATALOG=ON \
	-DANALYSIS_HASKELL=ON \
	..
