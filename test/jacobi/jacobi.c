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

#define N 650

double init_func(int x, int y) {
	return 40 * sin((double)(16 * (2 * x - 1) * y));
}

int main(int argc, char** argv) {

	clock_t start_t, end_t;
	double setup_time, elapsed_time;
	start_t = clock();

	// init matrix
	float u[N][N], tmp[N][N], f[N][N], res[N][N];
	memset(u, 0, N*N);    // 	initialize it with zeros

	// init F
	memset(f, 0, N*N);
	for (int i=0; i<N; i++)
		for (int j=0; j<N; j++)
			f[i][j] = init_func(i, j);

	double comm_time = 0;
	double comp_time = 0;
	double timer = 0;
	double resv;
	double factor = pow((double)1/N, 2);

	end_t = clock();
	setup_time =  (double)(end_t-start_t)/CLOCKS_PER_SEC;

	start_t = clock();
	for(int it=0; it<10; it++) {
		// main Jacobi loop
		#pragma omp parallel for
		for (int i=1; i < N-1; i++) {
			 for (int j=1; j < N-1; j++)
				 tmp[i][j] = (double)1/4 * (u[i-1][j] + u[i][j+1] + u[i][j-1] + u[i+1][j] - factor * f[i][j]);
		}
		memcpy(u, tmp, N*N);

		// calc the residuo
		for (int i=1; i<N-1; i++) {
			for (int j=1; j<N-1; j++)
				res[i][j] = f[i][j] - 4 * u[i][j] + u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1];
		}

		// normalize
		double norm = 0;
		#pragma omp for reduction(+: norm)
		for (int i=1; i<N-1; i++) {
			for (int j=1; j<N-1; j++)
				norm += pow( res[i][j], 2 );
		}

		resv = sqrt(norm)/(N-1);
	}

	end_t = clock();
	elapsed_time = (double)(end_t-start_t)/CLOCKS_PER_SEC;
	//printf("%d, %0.2f, %0.2f, %0.2f\n", N, setup_time + elapsed_time, setup_time, elapsed_time);
	//printf("%d\n", resv);
}
