#include <stdio.h>
#include <string.h>	
#include <math.h>

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
	
	printf("#3 test_valid_tile()\n");
	
	#pragma insieme tile(2,2,2)
	for (int i=0; i<4; i++) {
		for (int j=0; j<4; j++) {
			C[i][j] = 0;
			for(int k=0; k<4; k++) {
				C[i][j] += A[i][k] * A[k][i];
			}
		}
	}
	
	PRINT(C, 4, 4);
}

void test_valid_tile2() {

	float A[4][4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16}};
	float C[4][4];
	
	printf("#4 test_valid_tile2()\n");
	
	#pragma insieme tile(2,2,2)
	for (int i=1; i<3; i++) {
		for (int j=0; j<3; j++) {
			C[i+1][j] = 0;
			for(int k=0; k<4; k++) {
				C[i-1][j+1] += A[i][j] * A[j][i];
			}
		}
	}
	
	PRINT(C, 4, 4);
}

void test_invalid_tile() {

	float A[4][4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}, {13,14,15,16}};
	
	printf("#5 test_invalid_tile2()\n");
		
	#pragma insieme tile(2,2,2)
	for (int i=1; i<3; i++) {
		for (int j=i; j<3; j++) {
			A[i+1][j+1] = 0;
			for(int k=j; k<4; k++) {
				A[i-1][j+1] = A[i][k] * A[k][i];
			}
		}
	}
	
	PRINT(A, 4, 4);
}

void test_valid_fusion1() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#6 test_valid_fusion1()\n");

	#pragma insieme fuse(0,1)
	{
		for (int i=0; i<2; i++) {
			A[0][i] = A[1][i];
		}
	
		for (int j=0; j<3; j++) {
			A[1][j] = A[0][j];
		}
	}
	PRINT(A, 3, 3);
}

void test_valid_fusion2() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#7 test_valid_fusion2()\n");

	for(int k=0; k<10; k++) {
		#pragma insieme fuse(0,1)
		{
		for (int i=0; i<2; i++) {
			A[0][i] = A[1][i];
		}
	
		for (int j=0; j<3; j++) {
			A[1][j] = A[0][j];
		}
		}
	}
	PRINT(A, 3, 3);
}

void test_valid_fusion3() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
	float B[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#8 test_valid_fusion3()\n");

	#pragma insieme fuse(0,2)
	{
		for (int i=0; i<2; i++) {
			A[0][i] = A[1][i];
		}
		
		for (int i=0; i<2; i++) {
			for (int j=0; j<2; j++) {
				B[i][j] = A[0][i];
			}
		}
	
		for (int j=0; j<3; j++) {
			A[1][j] = A[0][j];
		}
	}
	PRINT(A, 3, 3);
	PRINT(B, 3, 3);
}

void test_valid_fusion4() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
	float B[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#9 test_valid_fusion4()\n");

	#pragma insieme fuse(1,2)
	{
		for (int i=0; i<2; i++) {
			A[0][i] = A[1][i];
		}
		
		for (int i=0; i<2; i++) {
			for (int j=0; j<2; j++) {
				B[i][j] = A[0][i];
			}
		}
	
		for (int j=0; j<3; j++) {
			A[1][j] = A[0][j];
		}
	}
	PRINT(A, 3, 3);
	PRINT(B, 3, 3);
}

void test_invalid_fusion1() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#10 test_invalid_fusion1()\n");

	#pragma insieme fuse(0,1)
	{
		for (int i=0; i<3; i++) {
			A[i][1] = A[i][0];
		}
	
		for (int j=0; j<3; j++) {
			A[1][j] = A[2][j];
		}
	}
	PRINT(A, 3, 3);
}

void test_invalid_fusion2() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#11 test_invalid_fusion2()\n");

	#pragma insieme fuse(0,1)
	{
		for (int i=0; i<3; i++) {
			A[i][1] = A[i][0];
		}

		A[1][1] = 0;
		
		for (int j=0; j<3; j++) {
			A[1][j] = A[2][j];
		}
	}
	PRINT(A, 3, 3);
}

void test_valid_fission() {

	float A[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};

	printf("#12 test_valid_fission1()\n");

	#pragma insieme split(1)
	for (int i=0; i<3; i++) {
		A[i][1] = A[2][0];
		A[1][0] = A[i][0];
	}
	PRINT(A, 3, 3);
}

int main(int argc, char* argv[]) {

	test_valid_interchange();
	test_invalid_interchange();
	
	test_valid_tile();
	test_valid_tile2();
	
	test_invalid_tile();
	
	test_valid_fusion1();
	test_valid_fusion2();
	test_valid_fusion3();
	test_valid_fusion4();
	test_invalid_fusion1();
	test_invalid_fusion2();
	
	test_valid_fission();
}
