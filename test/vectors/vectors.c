
#include <stdio.h>

#define N 10

int main() {

	// fixed size
	double a[N][N];
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			a[i][j] = 12;
		}
	}
	
	// dynamically sized
	int size = 12;
	double b[size][size];
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			b[i][j] = 12;
		}
	}
	
	return 0;
}
