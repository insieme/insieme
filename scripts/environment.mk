#
# This script should be included by all installation scripts to set up environment parameters. 
#

# determine installation directory
PREFIX = ${HOME}/libs

# determine number of cores to be used for compiling code (default = all)
SLOTS = `grep 'processor' /proc/cpuinfo | sort -u | wc -l`

# using default system compiler
CC  = gcc
CXX = g++

NEW_CXX	= ${PREFIX}/gcc-latest/bin/g++
NEW_CC	= ${PREFIX}/gcc-latest/bin/gcc

export CC
export CXX
export NEW_CC
export NEW_CXX
