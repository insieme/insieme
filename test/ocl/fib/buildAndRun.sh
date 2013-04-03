gcc -O3 fib.c -o fib -fopenmp -std=c99 -lOpenCL && time ./fib 30
