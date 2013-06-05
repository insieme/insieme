
# The directory used by cmake to locate third party libraries
export INSIEME_LIBS_HOME=~/libs 

# optional: set up the CC and CXX compiler to be used
#export CXX=g++
#export CC=gcc

# run cmake to create the project
cmake -D CMAKE_BUILD_TYPE=Debug ../ -DDOCS=on -DBUILD_DOXYGEN=on

