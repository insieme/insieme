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

bool mat_is_value(float *mat, unsigned elements, float value)
{
    for (unsigned i = 0; i < elements; ++i)
        if (mat[i] != value) return false;
    return true;
}

bool mat_init(float *mat, unsigned elements, float value)
{
	#pragma opencl device type(ALL)
	#pragma opencl requirement(mat, range(elements:0:elements), WO)
	#pragma opencl loop independent(yes)
	#pragma omp parallel for
	for (unsigned i = 0; i < elements; ++i)
	    mat[i] = value;

	return mat_is_value(mat, elements, value);
}

int main(int argc, char **argv)
{
    const int M = 1000;
    const int N = 1000;
    const int P = 1000;

    float *A = malloc(sizeof(*A)    * N * P);
    float *B = malloc(sizeof(float) * M * P);

    const size_t szFloat = sizeof(float);
    float *C = malloc(szFloat       * M * N);
    float *D = malloc(szFloat       * M * N);

    bool result = true;
    result &= mat_init(A, N * P, 2);
    result &= mat_init(B, M * P, 3);
    result &= mat_init(C, M * N, 0);
    result &= mat_init(D, M * N, 0);
    if (result) {
        // introduce some random bits
        for (unsigned i = 0; i < 1024; ++i) {
            A[rand() % (N*P)] = (rand() % 10) + 1;
            B[rand() % (M*P)] = (rand() % 10) + 1;
        }

				// now do the matrix multiplication
				#pragma opencl device type(ALL)
				#pragma opencl loop independent(yes)
				#pragma omp parallel for
				for (unsigned i = 0; i < N; ++i)
						#pragma opencl loop independent(yes)
				    for (unsigned j = 0; j < M; ++j)
				        #pragma opencl loop independent(no)
				        for (unsigned k = 0; k < P; ++k)
				            C[i*N+j] += A[i*P+k] * B[k*N+j];

		    // check with cpu if mat computation is correct
		    for (unsigned i = 0; i < N; ++i)
		        for (unsigned j = 0; j < M; ++j)
		            for (unsigned k = 0; k < P; ++k)
		                D[i*N+j] += A[i*P+k] * B[k*N+j];
		    result = mat_is_equal(C, D, M*N);
		}
    free(A);
    free(B);
    free(C);
    free(D);
    return result ? 0 : 1;
}
