CC = gcc
CFLAGS = -Wall -Wconversion -fPIC -DUNIX -std=c99 -g
INCLUDE = -I../microbenchs/cllib -I${OPENCL_ROOT}/include -I. -I../microbenchs -I/usr/lib/gcc/x86_64-linux-gnu/4.8/include
LIBS = -L${OPENCL_ROOT}/lib/x86_64 -lOpenCL

all: sparsevec

sparsevec:  sparsevec.c cltime.c
	$(CC) $(CFLAGS) sparsevec.c cltime.c $(INCLUDE) $(LIBS) -o sparsevec

clean:
	rm sparsevec
