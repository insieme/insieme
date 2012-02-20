
#include <stdlib.h>

#define N 500

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

	for(int step=0; step<1000; step++) {
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

}
	
