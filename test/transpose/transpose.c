/*
 * transpose.c
 *
 *  Created on: Nov 24, 2009
 *      Author: S. Pellegrini
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#include "ticktock.h"

#define MAX_VAL 100

#define IDX(M,R,C) (M[(R)*N+(C)])

//=============================================================================
//                              NAIVE TRANSPOSE
//-----------------------------------------------------------------------------
// This is the naive implementation of (in place) matrix transpose. No
// optimizations are done, the OpenMP version is straightforward.
void naive_transpose(double *M, int N){
	#pragma omp parallel for
	for(int i=0; i<N; i++){
		for(int j=i+1; j<N; j++){
			double tmp = IDX(M, i, j);
			IDX(M, i, j) = IDX(M, j, i);
			IDX(M, j, i) = tmp;
		}
	}
}

//=============================================================================
//                              BLOCK TRANSPOSE
//-----------------------------------------------------------------------------
// The block transpose divide the matrix into blocks of size BxB. At most 2
// brought to cache for transpose making it possible to optimize cache
// usage choosing an adeguate value of block.

// transposes a single block of size blockxblock
void transpose_block(double* M, int N, int block){
	for(int j=0; j<block; ++j)
		for(int k=j+1; k<block; ++k){
			double tmp = IDX(M,k,j);
			IDX(M,k,j) = IDX(M,j,k);
			IDX(M,j,k) = tmp;
		}
}

// transpose 2 opposite blocks by transferring data from block B1's columns
// to block B2's rows and vice-versa.
void transpose_opposite(double* M1, double* M2, int N, int block){
	for(int j=0; j<block; j++)
		for(int k=0; k<block; k++){
			double tmp = IDX(M1,k,j);
			IDX(M1,k,j) = IDX(M2,j,k);
			IDX(M2,j,k) = tmp;
		}
}

void block_transpose_rec(double *M, int N, int n, int block){
	
	assert(n>=block);

	if(n == block){
		#pragma omp task
		transpose_block(M, N, (n<block)?n:block);
		return;
	}

	// recur on the smaller problem size
	#pragma omp task
	block_transpose_rec(M+(1+N)*block, N, n-block, block);

	// transpose the corner block
	#pragma omp task
	transpose_block(M, N, block);
	for(int i=block; i<n; i+=block){
		// transpose opposite blocks
		#pragma omp task
		transpose_opposite(M+i, M+i*N, N, block);
	}
}

void block_transpose(double* M, size_t N, size_t block){
	#pragma omp parallel
	{
		#pragma omp single
		block_transpose_rec(M, N, N, block);
	}
}

void print(double* M, int N) {
	printf("====================\n");
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j)
			printf("%4.0f,", IDX(M,i,j));
		printf("\n");
	}
	printf("====================\n");
}

int doCheck(double* M, double *S, int N) {
	int check = 1;
	
	// check correctness 
	for(int i=0; check && i<N; ++i)
		for(int j=0; check && j<N; ++j) 
			check = IDX(S,i,j) == IDX(M,j,i);

	return check;
}

int main(int argc, char* argv[]) {

	int N;
	int B;

	if (argc != 3) {
		N = 2048;
		B = 16;
	} else {
		N = atoi(argv[1]);
		B = atoi(argv[2]);
	}

	assert(N%B==0 && "Matrix size not divisible by block size");

	double* M = (double*) malloc(sizeof(double) * N * N);
	double* S = (double*) malloc(sizeof(double) * N * N);

	// Initialize the values of the matrix 
	for (int i=0; i<N*N; ++i) { M[i] = rand()%MAX_VAL; }
	
	// print(M, N);
	
	// save the matrix for later check 
	memcpy(S, M, sizeof(double)*N*N);

#ifdef CMP_NAIVE
	// fill cache 
	naive_transpose(M, N);

	ticktock();
	for (int i=0; i<100; ++i) {
		naive_transpose(M, N);
	}
	long naive_time = ticktock();
	{
		int check = doCheck(M,S,N);
		printf("NAIVE CHECK %s\n", check?"OK":"FAILED");
	}
	printf("Elapsed time: %ld msecs\n", naive_time);

	// Put the matrix back 
	naive_transpose(M, N);
#endif

	size_t num_threads = 1;
#ifdef _OPENMP
	#pragma omp parallel
	num_threads = omp_get_num_threads();
#endif
	printf("Number of threads: %d\n", num_threads);

	block_transpose(M, N, B);

	ticktock();
	for(int i=0; i<100; ++i) {
		block_transpose(M, N, B);
	}
	long time = ticktock();

// 	print(M, N);
	{
		int check = doCheck(M,S,N);

		printf("CHECK %s\n", check?"OK":"FAILED");
	}
	printf("Elapsed time: %ld msecs\n", time);

#ifdef CMP_NAIVE
	printf("Speedup is: %lf\n", (double)naive_time/time);
#endif

	free(M);
	free(S);
}

