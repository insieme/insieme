#!/bin/bash

if [ $# -gt 0 ] ; then
	N=$1
else
	N=500
fi

echo "Running test with size $N ..."

echo "Compiling direct solution ..."
gcc -std=c99 indirection.c -g -DN=$N -o direct
echo "Compiling indirect solution ..."
gcc -std=c99 indirection.c -g -DN=$N -DINDIRECT -o indirect

echo "Compiling direct solution ..."
gcc -std=c99 indirection.c -g -DN=$N -O3 -o directO3
echo "Compiling indirect solution ..."
gcc -std=c99 indirection.c -g -DN=$N -DINDIRECT -O3 -o indirectO3


echo "Running direct version ..."
time ./direct

echo "Running indirect version ..."
time ./indirect

echo "Running direct version - optimized ..."
time ./directO3

echo "Running indirect version - optimized  ..."
time ./indirectO3

