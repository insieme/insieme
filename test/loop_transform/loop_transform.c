#include <stdio.h>
#include <string.h>	

#define PRINT(ARR, N, M) \
	for(unsigned i=0; i<N; ++i) { \
		for(unsigned j=0; j<M; ++j) { \
			printf("%f",ARR[i][j]);	\
			if (j<M-1) { printf(", "); } \
		} \
		printf("\n"); \
	} 


void test_valid_interchange() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#1 test_valid_interchage()\n");

	#pragma insieme interchange(0,1)
	for (int i=0; i<2; i++) {
		for (int j=0; j<2; j++) {
			A[i-1][j] = A[i][j];
		}
	}
	
	PRINT(A, 3, 3);
}

void test_invalid_interchange() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
	
	printf("#2 test_invalid_interchage()\n");
	
	#pragma insieme interchange(0,1)
	for (int i=0; i<2; i++) {
		for (int j=0; j<2; j++) {
			A[i-1][j+1] = A[i][j];
		}
	}
	
	PRINT(A, 3, 3);
}

void test_valid_tile() {

	float A[4][4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16}};
	float C[4][4];
	
	#pragma insieme tile(2,2,2)
	for (int i=0; i<4; i++) {
		for (int j=0; j<4; j++) {
			C[i][j] == 0;
			for(int k=0; k<4; k++) {
				C[i][j] += A[i][j] * A[j][i];
			}
		}
	}
	
	PRINT(A, 4, 4);
}

int main(int argc, char* argv[]) {

	test_valid_interchange();
	test_invalid_interchange();
	
	test_valid_tile();
	
}
