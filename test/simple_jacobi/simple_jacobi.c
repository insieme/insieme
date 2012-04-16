
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifndef N
	#define N 500
#endif

#ifndef M
	#define M 5
#endif

int main() {

	// allocate two copies of the processed array
	double (*A)[N][N] = malloc(sizeof(double[N][N]));
	double (*B)[N][N] = malloc(sizeof(double[N][N]));

	// fill input data into A
	for(int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			(*A)[i][j] = 0;
		}
	}

	// light a candle in the center
	(*A)[N/2][N/2] = 1;

	for(int step=0; step<M; step++) {
		for(int i=1; i<N-1; i++) {
			for(int j=1; j<N-1; j++) {
				(*B)[i][j] = (double)1/4 * ((*A)[i-1][j] + (*A)[i][j+1] + (*A)[i][j-1] + (*A)[i+1][j]);
			}
		}
		// switch sides
		double (*C)[N][N] = A;
		A = B;
		B = C;
	}

	// verification
	double sum = 0.0;
	int ok = 1;
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			sum += (*A)[i][j];
			if(labs(N/2-i) > M) {
				if((*A)[i][j] != 0.0) {
					ok = 0; 
					printf("FAIL B %d/%d\n", i,j); 
					exit(-1); 
				}
			}
		}
	}
	if(abs(sum-1) > 0.00001) { 
		ok = 0; printf("FAIL SUM %lf\n", fabs(sum-1)); exit(-1); 
	}

	printf("Verification: %s\n", ok?"OK":"FAILED");

}
	
