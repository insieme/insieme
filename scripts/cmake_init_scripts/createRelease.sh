# optional: set up the CC and CXX compiler to be used
#export CXX=g++
#export CC=gcc

# run cmake to create the project
../third_party/cmake/bin/cmake \
	-DOPENCL_ROOT=/path/to/opencl \
	-DCMAKE_BUILD_TYPE=Release \
	..
