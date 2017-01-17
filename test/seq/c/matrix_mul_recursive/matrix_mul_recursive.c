
#include <stdio.h>

#ifndef N
	#define N 1000
#endif

#define M N
#define K N

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
#define MAX(X,Y) ((X)>(Y)?(X):(Y))

#define VALUE double

// create the matices
VALUE A[N][M];
VALUE B[M][K];
VALUE C[N][K];


void mm_rec_sup(int li, int ui, int lj, int uj, int lk, int uk, long effort) {

	// conduct cut-off
	if (effort < 500) {

		// compute innermost
		for(int i=li; i<ui; i++) {
			for (int j=lj; j<uj; j++) {
				for (int k=lk; k<uk; k++)  {
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}

		// done
		return;
	}

	// process recursively
	effort = effort / 8;

	int mi = (ui + li) / 2;	
	int mj = (uj + lj) / 2;
	int mk = (uk + lk) / 2;

	// call recursive using gray code
	mm_rec_sup(li, mi, lj, mj, lk, mk, effort);
	mm_rec_sup(li, mi, lj, mj, mk, uk, effort);
	mm_rec_sup(li, mi, mj, uj, mk, uk, effort);
	mm_rec_sup(li, mi, mj, uj, lk, mk, effort);

	mm_rec_sup(mi, ui, mj, uj, lk, mk, effort);
	mm_rec_sup(mi, ui, mj, uj, mk, uk, effort);
	mm_rec_sup(mi, ui, lj, mj, mk, uk, effort);
	mm_rec_sup(mi, ui, lj, mj, lk, mk, effort);
}
		
void mm_rec() {
	mm_rec_sup(0,N,0,M,0,K,N*M*K);
}


int main(int argc, char** argv)  {

	#pragma omp parallel
	{
               
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<M; j++) {
				A[i][j] = i*j;
			}
		}

		// B is the identity matrix

		#pragma omp for
		for (int i=0; i<M; i++) {
			for (int j=0; j<K; j++) {
				B[i][j] = (i==j)?1:0;
			}
		}

		// process recursively
		mm_rec();

	}

	// verify result
	int success = 1;	
	for (int i=0; i<N; i++) {
		for (int j=0; j<MIN(M,K); j++) {
			if (A[i][j] != C[i][j]) {
				success = 0;
			}
		}
		for (int j=MIN(M,K); j<MAX(M,K); j++) {
			if (C[i][j] != 0) {
				success = 0;
			}
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");
}
