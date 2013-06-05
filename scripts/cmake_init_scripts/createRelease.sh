
# The directory used by cmake to locate third party libraries
INSIEME_LIBS_HOME=~/libs 

# optional: set up the CC and CXX compiler to be used
#CXX=g++
#CC=gcc

# run cmake to create the project
cmake -D CMAKE_BUILD_TYPE=Release ../ -DDOCS=off
