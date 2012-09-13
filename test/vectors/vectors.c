
#include <stdio.h>

#define N 10


void funA(double* A) {
	printf("%f\n", A[5*N+5]);
}

void funB(double** A) {
	printf("%f\n", A[5][5]);
}


int main() {

	// fixed size
	double a[N][N];
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			a[i][j] = i*1000+j;
		}
	}
	
	// dynamically sized
	int size = N;
	double b[size][size];
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			b[i][j] = i*1000+j;
		}
	}
	
/*
	funA(a);
	funB(a);
	funA(b);
	funB(b);
*/
	funA((double*)b);

	return 0;
}
