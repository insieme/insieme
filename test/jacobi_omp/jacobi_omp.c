/*
 * jacobi_mpi.cpp
 *
 * Standard SEND/RECV
 *
 *  Created on: Sep 12, 2008
 *      Author: spellegrini
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

double init_func(int x, int y) {
	return 40 * sin((double)(16 * (2 * x - 1) * y));
}

int main(int argc, char** argv) {
	clock_t start_t, end_t;
	double setup_time, elapsed_time;
	start_t = clock();

	int N = 650;
	if (argc > 1) {
		N = atoi(argv[1]);
	}

	int numIter = 100;
	if (argc > 2) {
		numIter = atoi(argv[1]);
	}
	

	// init matrix
	float* u;
	float* tmp;
	float* f;
	float* res;

	u = malloc(N*N*sizeof(float));
	tmp = malloc(N*N*sizeof(float));
	f = malloc(N*N*sizeof(float));
	res = malloc(N*N*sizeof(float));

	if (!(u && tmp && f && res)) {
		printf("Error allocating arrays\n");
	}

	// initialize it with zeros
	memset(u, 0, N*N*sizeof(float));    

	// init F
	memset(f, 0, N*N*sizeof(float));
	for (int i=0; i<N; i++)
		for (int j=0; j<N; j++)
			f[i*N+j] = init_func(i, j);

	double comm_time = 0;
	double comp_time = 0;
	double timer = 0;
	double resv;
	double factor = pow((double)1/N, 2);

	end_t = clock();
	setup_time =  (double)(end_t-start_t)/CLOCKS_PER_SEC;

	start_t = clock();
	for(int it=0; it<numIter; it++) {
		// main Jacobi loop

		#pragma omp parallel
		{			
			#pragma omp for
			for (int i=1; i < N-1; i++) {
				for (int j=1; j < N-1; j++) {
					tmp[i*N+j] = (double)1/4 * (u[(i-1)*N+j] + u[i*N+j+1] + u[i*N+j-1] + u[(i+1)*N+j] - factor * f[i*N+j]);
				}
			}
		}
		memcpy(u, tmp, N*N*sizeof(float));

		// calc the residuo
		for (int i=1; i<N-1; i++) {
			for (int j=1; j<N-1; j++)
				res[i*N+j] = f[i*N+j] - 4 * u[i*N+j] + u[(i-1)*N+j] + u[(i+1)*N+j] + u[i*N+j-1] + u[i*N+j+1];
		}

		// normalize
		double norm = 0;
		//#pragma omp for reduction(+: norm)
		for (int i=1; i<N-1; i++) {
			for (int j=1; j<N-1; j++) {
				norm += pow( res[i*N+j], 2 );
			}
		}

					
		resv = sqrt(norm)/(N-1);
	}

	end_t = clock();
	elapsed_time = (double)(end_t-start_t)/CLOCKS_PER_SEC;
	// printf("%d, %0.2f, %0.2f, %0.2f\n", N, setup_time + elapsed_time, setup_time, elapsed_time);

	// free memory
	free(u);
	free(tmp);
	free(f);
	free(res);

	// add some output to make correct execution distinguishable from segmentation fault
	printf("Job Done! - residuo: %lf\n", resv);
}
