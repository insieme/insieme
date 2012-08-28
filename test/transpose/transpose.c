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

//=============================================================================
//								NAIVE TRANSPOSE
//-----------------------------------------------------------------------------
// This is the naive implementation of (in place) matrix transpose. No
// optimizations are done, the OpenMP version is straightforward.
void naive_transpose(double *M, size_t N){
	int i, j;
	#pragma omp parallel for private(i,j)
	for(i=0; i<N; i++){
		for(j=i; j<N; j++){
			double tmp = M[i*N + j];
			M[i*N + j] = M[j*N + i];
			M[j*N + i] = tmp;
		}
	}
}

//=============================================================================
//								BLOCK TRANSPOSE
//-----------------------------------------------------------------------------
// The block transpose divide the matrix into blocks of size BxB. At most 2
// brought to cache for transpose making it possible to optimize cache
// usage choosing an adeguate value of block.

// transposes a single block of size blockxblock
void transpose_block(double* M, int N, int block){
	int j,k;
	for(j=0; j<block; j++)
		for(k=j; k<block; k++){
			double tmp = M[k*N + j];
			M[k*N + j] = M[j*N + k];
			M[j*N + k] = tmp;
		}
}

// transpose 2 opposite blocks by transferring data from block B1's columns
// to block B2's rows and vice-versa.
void transpose_opposite(double* M1, double* M2, int N, int block){
	int j,k;
	for(j=0; j<block; j++)
		for(k=0; k<block; k++){
			double tmp = M1[k*N + j];
			M1[k*N + j] = M2[j*N + k];
			M2[j*N + k] = tmp;
		}
}

void block_transpose_rec(double *M, size_t N, size_t n, size_t block){
	if(n <= block){
		#pragma omp task
		transpose_block(M, N, (n < block)?n:block);
		return;
	}
	
	// transpose the corner block
	#pragma omp task
	transpose_block(M, N, block);
	int i;
	for(i=1; i<n/block; i++){
		// transpose opposite blocks
		#pragma omp task
		transpose_opposite(M + i*block, M + i*block*N, N, block);
	}
	// recur on the smaller problem size
	block_transpose_rec(M+(1+N)*block, N, n-block, block);
}

void block_transpose(double* M, size_t N, size_t block){
	#pragma omp parallel
	{
		#pragma omp single
		block_transpose_rec(M, N, N, block);
	}
}

int main(int argc, char* argv[]) {

	size_t N;
	if (argc == 1)
		N = 8192;
	else
		N = atoi(argv[1]);

	double* M = (double*) malloc(sizeof(double) * N * N);
	double* S = (double*) malloc(sizeof(double) * N * N);

	// Initialize the values of the matrix 
	for (int i=0; i<N*N; ++i) { M[i] = rand()%MAX_VAL; }

	// save the matrix for later check 
	memcpy(S, M, sizeof(double)*N*N);

	size_t num_threads = 0;
#ifdef OPENMP
	#pragma omp parallel
	num_threads = omp_get_num_threads();
#endif
	ticktock();
	block_transpose(M, N, 32);
	long time = ticktock();

	int check = 1;
	// check correctness 
	for(int i=0; i<N && check; ++i)
		for(int j=0; j<N && check; ++j) 
			check = S[i*N+j]==M[j*N+i];
	
	printf("CHECK %s\n", check?"OK":"FAILED");
	printf("Elapsed time: %ld msecs\n", time);

	free(M);
	free(S);
}

