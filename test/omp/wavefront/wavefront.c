
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

#define N 400

double mat_a[N+1][N+1];

int main(int argc, char** argv) {
	srand(10);
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			mat_a[i][j] = (double)rand() / (double)RAND_MAX * 200.0;
		}
	}		

// 	int converged = 0;
	//double t = omp_get_wtime();
	#pragma omp parallel 
	for(int iter=0; iter<400; ++iter) {
		#pragma omp for schedule(dynamic,32)
		for(int i=1; i<2*N-2; ++i) {
			if(i<N) {
				for(int j=1; j<=i; ++j) {
					int x = i-j+1;
					double old = mat_a[x][j];
					mat_a[x][j] = 0.8*old + 0.2*0.25*mat_a[x-1][j]*mat_a[x+1][j]*mat_a[x][j-1]*mat_a[x][j+1];
// 					diff += fabs(mat_a[x][j] - old);
				}
			} else {
				for(int j=i-N+2; j<N; ++j) {
					int x = i-j+1;
					double old = mat_a[x][j];
					mat_a[x][j] = 0.9*old + 0.1*0.25*mat_a[x-1][j]*mat_a[x+1][j]*mat_a[x][j-1]*mat_a[x][j+1];
// 					diff += fabs(mat_a[x][j] - old);
				}
			}
		}
// 		if(diff < 0.000001) converged = 1;
	}
	//printf("Complete: %8.4lf\n", omp_get_wtime()-t);
}
