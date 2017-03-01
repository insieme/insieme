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
/*
 * jacobi_mpi.cpp
 *
 * Standard SEND/RECV
 *
 *  Created on: Sep 12, 2008
 *      Author: spellegrini
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>

double init_func(int x, int y) {
	return 40 * sin((double)(16 * (2 * x - 1) * y));
}

#define N 5

int main(int argc, char** argv) {
	clock_t start_t, end_t;
	double setup_time, elapsed_time;
	start_t = clock();

	// init matrix
	float u[N][N], tmp[N][N], f[N][N], res[N][N];
	memset(u, 0, N * N); // 	initialize it with zeros

	// init F
	{
		memset(f, 0, N * N);
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				f[i][j] = init_func(i, j);
			}
		}
	}

	double comm_time = 0;
	double comp_time = 0;
	double timer = 0;
	double resv;
	double factor = pow((double)1 / N, 2);

	end_t = clock();
	setup_time = (double)(end_t - start_t) / CLOCKS_PER_SEC;

	#ifdef DDEBUG
	printf("Setup finished! (setup time: %0.2f)\n", setup_time);
	#endif
	start_t = clock();

	for(int it = 0; it < 100; it++) {
	// main Jacobi loop
	#pragma omp parallel for private(resv) reduction(+ : resv)
		for(int i = 1; i < N - 1; i++) {
			for(int j = 1; j < N - 1; j++) {
				tmp[i][j] = (double)1 / 4 * (u[i - 1][j] + u[i][j + 1] + u[i][j - 1] + u[i + 1][j] - factor * f[i][j]);
			}
		}
		#pragma omp critical(pippo)
		memcpy(u, tmp, N * N);

		// calc the residuo
		for(int i = 1; i < N - 1; i++) {
			for(int j = 1; j < N - 1; j++) {
				res[i][j] = f[i][j] - 4 * u[i][j] + u[i - 1][j] + u[i + 1][j] + u[i][j - 1] + u[i][j + 1];
			}
		}

		// normalize
		double norm = 0;
		#pragma omp for reduction(+ : norm)
		for(int i = 1; i < N - 1; i++) {
			for(int j = 1; j < N - 1; j++) {
				norm += pow(res[i][j], 2);
			}
		}

		resv = sqrt(norm) / (N - 1);
		#pragma omp barrier
		#pragma omp flush norm
		#pragma omp barrier
	}

	end_t = clock();
	elapsed_time = (double)(end_t - start_t) / CLOCKS_PER_SEC;

	printf("%d, %0.2f, %0.2f, %0.2f\n", N, setup_time + elapsed_time, setup_time, elapsed_time);
}
