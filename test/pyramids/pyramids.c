#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include <assert.h>

// define the problem size
#ifndef P
	#define P 7
#endif

// has to be one of 2^n + 1
#define N ((1<<P)+1)

// defines the number of iterations
#define M ((N-1)/2)


// define the cut-off value
#ifndef CUT
	#define CUT 6
#endif

#define CUT_OFF ((1<<CUT)-1)

// the type of value the computation should be based on
#ifndef VALUE_TYPE
	#define VALUE_TYPE double
#endif

// enables debugging messages
#ifndef DEBUG
	#define DEBUG 0
#endif

typedef VALUE_TYPE real;
typedef real Grid[N][N];

#define min(A,B) (((A)<(B))?(A):(B))

void printGrid(Grid* A) {
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			printf((A[0][i][j]!=0.0)?"+":"-");
			//printf("%d",((int)A[0][i][j])%10);
		}
		printf("\n");
	}
	printf("Center: %f\n", A[0][N/2][N/2]);
}

// -----------------------------------------------------
// Stencil Kernel
// -----------------------------------------------------

int steps;

void update(Grid* A, Grid* B, int i, int j) {

	// correct coordinates
	i = (i+(N-1)) % (N-1);
	j = (j+(N-1)) % (N-1);

//	printf("Location: %d/%d\n", i, j);

	// test for edge coordinates
	if (i==0 || j == 0 || i==N-1 || j==N-1) {
		return;		// nothing to do on edge (so far)
	}

	(*B)[i][j] = (double)1/4 * ((*A)[i-1][j] + (*A)[i][j+1] + (*A)[i][j-1] + (*A)[i+1][j]);

//	// ---- Debugging ----
//
//	int s = A[0][i][j];
//	assert(0 <= i && i < N);
//	assert(0 <= j && j < N);
//
//	steps++;
//
//	if (
//			(i!=1 && A[0][i-1][j] != s) ||
//			(j!=1 && A[0][i][j-1] != s) ||
//			(i!=N-2 && A[0][i+1][j] != s) ||
//			(j!=N-2 && A[0][i][j+1] != s) ||
//			A[0][i][j] != s) {
//
//		printf("Input data not valid: %d / %d / %d / %d / %d - should: %d\n",
//				(int)A[0][i-1][j], (int)A[0][i][j-1], (int)A[0][i+1][j], (int)A[0][i][j+1], (int)A[0][i][j], s);
//
//		printf("Step %d: Problem %d/%d/%d ...\n", steps, i, j, s+1);
//		printf("\nA:\n"); printGrid(A);
//		printf("\nB:\n"); printGrid(B);
//		assert(0);
//	}
//
//	if (B[0][i][j] >= s + 1) {
//		printf("Step %d: Re-computing point %d/%d/%d\n", steps, i, j, s+1);
//		printf("\nA:\n"); printGrid(A);
//		printf("\nB:\n"); printGrid(B);
//		assert(0);
//	}
//
//	// count updates
//	//printf("%d - Computing (%d,%d,%d)\n", steps++, i, j, s+1);
//	B[0][i][j] = s + 1;

}



// -----------------------------------------------------
// Iterative Computation
// -----------------------------------------------------


void jacobi_iterative(Grid* A, Grid* B, int num_iter) {
	#pragma omp parallel
	for(int step=0; step<num_iter; step++) {
		#pragma omp for
		for(int i=1; i<N-1; i++) {
			for(int j=1; j<N-1; j++) {
				update(A,B,i,j);
			}
		}
		// switch sides
		Grid* C = A;
		A = B;
		B = C;
	}
}


// -----------------------------------------------------
// Recursive Computation
// -----------------------------------------------------



// computes a pyramid types
void compute_pyramid(Grid* A, Grid* B, int x, int y, int h);
void compute_reverse(Grid* A, Grid* B, int x, int y, int h);

// computes a wedge (piece between pyramids - x base line points in x direction)
void compute_wedge_x(Grid* A, Grid* B, int x, int y, int h);
void compute_wedge_y(Grid* A, Grid* B, int x, int y, int h);


/**
 * Computes the pyramid with center point (x,y) of size s (edge size, must be odd)
 */
void compute_pyramid(Grid* A, Grid* B, int x, int y, int s) {
	assert(s % 2 == 1 && "Only odd sizes are supported!");
	//assert(x >= s && y >= s && "Coordinates not matching!");

	// compute height of pyramid

	if (s <= CUT_OFF) {

		int h = (s + 1) / 2;
		if (DEBUG) printf("Computing pyramid at (%d,%d) with size %d and %d levels ...\n", x, y, s, h);

		// just compute the pyramid
		for(int l=h-1; l>=0; l--) {

			// compute one plain of the pyramid
			for(int i = x-l; i<=x+l; i++) {
				for(int j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			Grid* C = A;
			A = B;
			B = C;
		}

		// done
		return;
	}

	if (DEBUG) printf("Decomposing pyramid at (%d,%d) with size %d ...\n", x, y, s);

	// cut into 6 smaller pyramids + 4 wedges

	// compute size of sub-pyramids
	int d = (s-1)/2;
	int h = (d+1)/2;

	int ux = x - h;
	int lx = x + h;
	int uy = y - h;
	int ly = y + h;

	// compute 4 base-pyramids (parallel)
	#pragma omp task
	compute_pyramid(A, B, ux, uy,  d);
	#pragma omp task
	compute_pyramid(A, B, ux, ly,  d);
	#pragma omp task
	compute_pyramid(A, B, lx, uy,  d);
	#pragma omp task
	compute_pyramid(A, B, lx, ly,  d);

	#pragma omp taskwait

	// compute 4 wedges (parallel)
	#pragma omp task
	compute_wedge_x(A, B, ux, y, d);
	#pragma omp task
	compute_wedge_x(A, B, lx, y, d);
	#pragma omp task
	compute_wedge_y(A, B, x, uy, d);
	#pragma omp task
	compute_wedge_y(A, B, x, ly, d);

	#pragma omp taskwait

	// compute reverse pyramid in the center
	compute_reverse(A, B, x, y, d);

	// compute tip
	compute_pyramid(A, B, x, y, d);

}


void compute_reverse(Grid* A, Grid* B, int x, int y, int s) {
	assert(s % 2 == 1 && "Only odd sizes are supported!");

	// check for terminal case
	if (s <= CUT_OFF) {

		// compute height of pyramid
		int h = (s + 1) / 2;
		if (DEBUG) printf("Computing reverse pyramid at (%d,%d) with size %d  ...\n", x, y, s);

		// just compute the pyramid
		for(int l=0; l<h; l++) {

			// compute one plain of the pyramid
			for(int i = x-l; i<=x+l; i++) {
				for(int j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			Grid* C = A;
			A = B;
			B = C;
		}

		// done
		return;
	}

	if (DEBUG) printf("Decomposing reverse pyramid at (%d,%d) with size %d ...\n", x, y, s);

	// cut into 6 smaller pyramids + 4 wedges

	// compute size of pyramids
	int d = (s-1)/2;
	int h = (d+1)/2;

	int ux = x - h;
	int lx = x + h;
	int uy = y - h;
	int ly = y + h;

	// compute tip
	compute_reverse(A, B, x, y, d);

	// compute reverse pyramid in the center
	compute_pyramid(A, B, x, y, d);

	// compute 4 wedges (parallel)
	#pragma omp task
	compute_wedge_y(A, B, ux, y, d);
	#pragma omp task
	compute_wedge_y(A, B, lx, y, d);
	#pragma omp task
	compute_wedge_x(A, B, x, uy,  d);
	#pragma omp task
	compute_wedge_x(A, B, x, ly, d);

	#pragma omp taskwait

	// compute 4 base-pyramids (parallel)
	#pragma omp task
	compute_reverse(A, B, lx, ly,  d);
	#pragma omp task
	compute_reverse(A, B, lx, uy,  d);
	#pragma omp task
	compute_reverse(A, B, ux, ly,  d);
	#pragma omp task
	compute_reverse(A, B, ux, uy,  d);

	#pragma omp taskwait

}


void compute_wedge_x(Grid* A, Grid* B, int x, int y, int s) {
	assert(s > 0);

	if (s <= CUT_OFF) {

		// compute height of wedge
		int h = (s + 1) / 2;

		if (DEBUG) printf("Computing wedge %d/%d/%d/X - height %d ...\n", x, y, s, h);

		// just compute the wedge
		for(int l=0; l<h; l++) {

			// compute one level of the wedge
			for(int i = x-(h-l)+1; i<=x+(h-l)-1; i++) {
				for(int j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			Grid* C = A;
			A = B;
			B = C;
		}

		return;
	}

	if (DEBUG) printf("Decomposing wedge %d/%d/%d/X ...\n", x, y, s);

	// decompose into 2 pyramids and 4 wedges

	// compute coordinates offset of sub-wedges
	int d = (s-1)/2;
	int h = (d+1)/2;

	// compute bottom wedges (parallel)
	#pragma omp task
	compute_wedge_x(A, B, x-h, y, d);
	#pragma omp task
	compute_wedge_x(A, B, x+h, y, d);
	#pragma omp taskwait

	// reverse pyramid
	compute_reverse(A, B, x, y, d);

	// compute pyramid on top
	compute_pyramid(A, B, x, y, d);

	// compute remaining two wedges (parallel)
	#pragma omp task
	compute_wedge_x(A, B, x, y-h, d);
	#pragma omp task
	compute_wedge_x(A, B, x, y+h, d);
	#pragma omp taskwait

}

void compute_wedge_y(Grid* A, Grid* B, int x, int y, int s) {
	assert(s > 0);


	if (s <= CUT_OFF) {

		// compute height of wedge
		int h = (s + 1) / 2;

		if (DEBUG) printf("Computing wedge %d/%d/%d/Y ...\n", x, y, s);

		// just compute the wedge
		for(int l=0; l<h; l++) {

			// compute one plain of the pyramid
			for(int i = x-l; i<=x+l; i++) {
				//printf("Level %d - bounds x: %d,%d - bounds y: %d,%d\n", h, x-l, x+l, y-(h-l)+1, y+(h-l)-1 );
				for(int j = y-(h-l)+1; j<=y+(h-l)-1; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			Grid* C = A;
			A = B;
			B = C;
		}

		return;
	}

	if (DEBUG) printf("Decomposing wedge %d/%d/%d/Y ...\n", x, y, s);

	// decompose into 2 pyramids and 4 wedges

	// compute coordinates offset of sub-wedges
	int d = (s-1)/2;
	int h = (d+1)/2;

	// compute bottom wedges (parallel)
	#pragma omp task
	compute_wedge_y(A, B, x, y-h, d);
	#pragma omp task
	compute_wedge_y(A, B, x, y+h, d);
	#pragma omp taskwait

	// reverse pyramid
	compute_reverse(A, B, x, y, d);

	// compute pyramid on top
	compute_pyramid(A, B, x, y, d);

	// compute remaining two wedges (parallel)
	#pragma omp task
	compute_wedge_y(A, B, x-h, y, d);
	#pragma omp task
	compute_wedge_y(A, B, x+h, y, d);
	#pragma omp taskwait
}



void jacobi_recursive(Grid* A, Grid* B, int num_iter) {

	#pragma omp parallel
	{
		#pragma omp single
		{
			// compute full pyramid
			if (DEBUG) printf("\nProcessing main pyramid ...\n");
			compute_pyramid(A, B, N/2, N/2, N-2);
			if (DEBUG) printf("\nProcessing x wedge ...\n");
			compute_wedge_x(A, B, N/2,0, N-2);
			if (DEBUG) printf("\nProcessing y wedge ...\n");
			compute_wedge_y(A, B, 0, N/2, N-2);
			if (DEBUG) printf("\nProcessing reverse pyramid ...\n");
			compute_reverse(A, B, 0, 0, N-2);
		}
	}

}


int main() {

	// allocate two copies of the processed array
	Grid* A = malloc(sizeof(Grid));
	Grid* B = malloc(sizeof(Grid));

	// fill input data into A
	for(int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			(*A)[i][j] = 0;
		}
	}

	// light a candle in the center
	(*A)[N/2][N/2] = 1;

	printf("Running problem size %d = %d x %d ...\n", P, N, M);

	// run computation
#ifdef _OPENMP
	double start = omp_get_wtime();
#endif
#ifdef iterative
	jacobi_iterative(A,B,M);
#else
	printf("Using CUT = %d => CUT_OFF = %d\n", CUT, CUT_OFF);
	jacobi_recursive(A,B,M);
#endif
#ifdef _OPENMP
	double time = omp_get_wtime() - start;
	printf("Execution time: %.1fms\n", time*1000);
#endif

	// verification
	double sum = 0.0;
	int ok = 1;
	for(int i=0; i<N && ok; ++i) {
		for(int j=0; j<N && ok; ++j) {
			sum += A[0][i][j];
			if(labs(N/2-i) > M && A[0][i][j] != 0.0) {
				ok = 0;
				printf("FAIL B %d/%d\n", i,j);
			}
		}
	}
	if(ok && abs(sum-1) > 0.00001) {
		ok = 0;
		printf("FAIL SUM %lf\n", fabs(sum-1));
	}

	//	printf("\nA:\n"); printGrid(A);
	//	printf("\nB:\n"); printGrid(B);

	printf("Verification: %s\n", ok?"OK":"FAILED");
	return (ok)?EXIT_SUCCESS:EXIT_FAILURE;

}
