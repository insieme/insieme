#
# This script should be included by all installation scripts to set up environment parameters. 
#

# determine installation directory
PREFIX = ~/libs

# determine number of cores to be used for compiling code (default = all)
SLOTS = `grep 'processor' /proc/cpuinfo | sort -u | wc -l`

# using default system compiler
CC  = gcc
CXX = g++

