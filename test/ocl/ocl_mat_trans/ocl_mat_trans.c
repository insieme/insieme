#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>

bool mat_is_equal(float *fst, float *snd, unsigned elements)
{
    for (unsigned i = 0; i < elements; ++i) {
        if ((unsigned) fst[i] != (unsigned) snd[i]) return false;
	}
    return true;
}

bool mat_init(float *mat, unsigned N, unsigned M)
{
    #pragma opencl device type(ALL)
    #pragma opencl requirement(mat, range(N*M:0:N*M), WO)
    #pragma opencl loop independent(yes)
	#pragma omp parallel for
    for (unsigned i = 0; i < N; ++i) {
		for (unsigned  j = 0; j < M; ++j) {
			mat[i*M+j] = i + 10 * j;
		}
	}
    return true;
}

int main(int argc, char **argv)
{
    const int M = 1000;
    const int N = 1000;

    float *A = malloc(sizeof(*A)    * M * N);
    float *B = malloc(sizeof(float) * M * N);
		float *C = malloc(sizeof(float) * M * N);

    bool result = mat_init(A, M, N);
    if (result) {
        // now do the matrix multiplication
        #pragma opencl device type(ALL)
        #pragma opencl loop independent(yes)
				#pragma omp parallel for
        for (unsigned i = 0; i < N; ++i) {
						#pragma opencl loop independent(yes)
						for (unsigned j = 0; j < M; ++j) {
							B[i*M+j] = A[j*N+i];
						}
				}

        // check with cpu if mat computation is correct
        for (unsigned i = 0; i < N; ++i) {
						for (unsigned j = 0; j < M; ++j) {
							C[i*M+j] = A[j*N+i];
						}
				}

        result = mat_is_equal(B, C, M*N);
    }

    free(A);
    free(B);
    free(C);
    return result ? 0 : 1;
}
