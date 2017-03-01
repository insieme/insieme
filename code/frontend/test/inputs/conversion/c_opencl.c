/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>

#define TIMEX_BGN(name) \
	{ \
		const char *__name = name; \
		struct timeval __t1, __t2; \
		double __elapsed; \
		gettimeofday(&__t1, NULL); \
		{

#define TIMEX_END() \
		} \
		gettimeofday(&__t2, NULL); \
		__elapsed  = (__t2.tv_sec  - __t1.tv_sec) * 1000.0; \
		__elapsed += (__t2.tv_usec - __t1.tv_usec) / 1000.0; \
		fprintf(stdout, "%s: %.3f ms\n", __name, __elapsed); \
	}

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
	TIMEX_BGN("mat_init_acc");
    #pragma opencl device type(ALL)
    #pragma opencl requirement(mat, range(elements:0:elements), WO)
    #pragma opencl loop independent(yes)
	#pragma omp parallel for
    for (unsigned i = 0; i < elements; ++i)
        mat[i] = value;
	TIMEX_END();
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

		TIMEX_BGN("mat_mult_acc");
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
		TIMEX_END();

		TIMEX_BGN("mat_mult_seq");
        // check with cpu if mat computation is correct
        for (unsigned i = 0; i < N; ++i)
            for (unsigned j = 0; j < M; ++j)
                for (unsigned k = 0; k < P; ++k)
                    D[i*N+j] += A[i*P+k] * B[k*N+j];
		TIMEX_END();
        result = mat_is_equal(C, D, M*N);
    }

    free(A);
    free(B);
    free(C);
    free(D);
    return result ? 0 : 1;
}
