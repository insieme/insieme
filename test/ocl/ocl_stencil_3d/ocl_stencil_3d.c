#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>

#ifndef N
#define N 201
#endif

#ifndef K
#define K N
#endif

#ifndef M
#define M 1
#endif

#ifndef STENCIL_SIZE
#define STENCIL_SIZE 9
#endif
#define STENCIL_SIZE2 (STENCIL_SIZE/2)

#define INIT_VAL 100.0

#define AT(p, i, j, k) p[((i)*N*(K+1)) + ((j)*(K+1)) + (k)]

int main(int argc, char **argv)
{
	int numOfElements = N*N*(K+1);
	double *A = malloc(sizeof(*A) * numOfElements);
	double *B = malloc(sizeof(*B) * numOfElements);
	for (unsigned i = 0; i < N; ++i)
		for (unsigned j = 0; j < N; ++j)
			for (unsigned k = 0; k < K; ++k) {
				bool cond = true;
				cond &= (i == N/2);
				cond &= (j == N/2);
				cond &= (k == K/2);
				AT(A, i, j, k) = cond ? INIT_VAL : 0.0;
				AT(B, i, j, k) = 0.0;
			}

	double stencil[STENCIL_SIZE][STENCIL_SIZE][STENCIL_SIZE];
	{
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					double id = (i+0.5) - STENCIL_SIZE/2.0;
					double jd = (j+0.5) - STENCIL_SIZE/2.0;
					double kd = (k+0.5) - STENCIL_SIZE/2.0;
					stencil[i][j][k] = sqrt(id*id + jd*jd + kd*kd);
				}
			}
		}
		double stencil_base = stencil[0][0][0]+0.0001;
		double stencil_sum = 0.0;
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					stencil[i][j][k] = stencil_base - stencil[i][j][k];
					stencil_sum += stencil[i][j][k];
				}
			}
		}
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					stencil[i][j][k] /= stencil_sum;
				}
			}
		}
	}

	double *curr = A;
	double *next = B;
	for(int iter=0; iter<M; iter++) {
		#pragma opencl device type(ALL)
		#pragma opencl loop independent(yes) collapse(2)
		#pragma opencl requirement(curr, range(numOfElements:0:numOfElements), RW)
		#pragma opencl requirement(next, range(numOfElements:0:numOfElements), RW)
		#pragma omp parallel for
		for(int i=STENCIL_SIZE2; i<N-STENCIL_SIZE2; ++i) {
			for(int j=STENCIL_SIZE2; j<N-STENCIL_SIZE2; ++j) {
				for(int k=STENCIL_SIZE2; k<K-STENCIL_SIZE2; ++k) {
					AT(next,i,j,k) = 0.0;
					for(int si=-STENCIL_SIZE2; si<=STENCIL_SIZE2; ++si) {
						for(int sj=-STENCIL_SIZE2; sj<=STENCIL_SIZE2; ++sj) {
							for(int sk=-STENCIL_SIZE2; sk<=STENCIL_SIZE2; ++sk) {
								AT(next,i,j,k) += AT(curr,i+si,j+sj,k+sk) * stencil[si+STENCIL_SIZE2][sj+STENCIL_SIZE2][sk+STENCIL_SIZE2];
							}
						}
					}
				}
			}
		}

		double *tmp = curr;
		curr = next;
		next = tmp;
	}

	double sum = 0.0;
	int ok = 1;
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			for(int k=0; k<K; ++k) {
				sum += AT(curr,i,j,k);
				if(labs(N/2-i) > M*STENCIL_SIZE2) {
					if(AT(curr,i,j,k)!= 0.0) {
						ok = 0;
					}
				}
			}
		}
	}
	if(abs(sum-INIT_VAL) > 0.00001) {
		ok = 0;
	}

	free(A);
	free(B);
  return !ok;
}
