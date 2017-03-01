#include <cilk/cilk.h>

#include <stdlib.h>
#include <stdio.h>
#include <math.h>


// ---------------------------------------------

#include <assert.h>

// define the problem size
#ifndef P
	#define P 9
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

// defines the cut-off for the recursive processing
#ifndef CUT_OFF
	#define CUT_OFF 8
#endif

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
	int i, j;
	for(i=0; i<N; i++) {
		for(j=0; j<N; j++) {
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
	int l, i, j, d, h, ux, lx, uy, ly;
	assert(s % 2 == 1 && "Only odd sizes are supported!");
	//assert(x >= s && y >= s && "Coordinates not matching!");

	// compute height of pyramid

	if (s <= CUT_OFF) {

		int h = (s + 1) / 2;
		if (DEBUG) printf("Computing pyramid at (%d,%d) with size %d and %d levels ...\n", x, y, s, h);
		
		// just compute the pyramid
		for(l=h-1; l>=0; l--) {
			Grid* C;

			// compute one plain of the pyramid
			for(i = x-l; i<=x+l; i++) {
				for(j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			C = A;
			A = B;
			B = C;
		}

		// done
		return;
	}

	if (DEBUG) printf("Decomposing pyramid at (%d,%d) with size %d ...\n", x, y, s);

	// cut into 6 smaller pyramids + 4 wedges

	// compute size of sub-pyramids
	d = (s-1)/2;
	h = (d+1)/2;

	ux = x - h;
	lx = x + h;
	uy = y - h;
	ly = y + h;

	// compute 4 base-pyramids (parallel)
	cilk_spawn compute_pyramid(A, B, ux, uy,  d);
	cilk_spawn compute_pyramid(A, B, ux, ly,  d);
	cilk_spawn compute_pyramid(A, B, lx, uy,  d);
	cilk_spawn compute_pyramid(A, B, lx, ly,  d);

	cilk_sync;

	// compute 4 wedges (parallel)
	cilk_spawn compute_wedge_x(A, B, ux, y, d);
	cilk_spawn compute_wedge_x(A, B, lx, y, d);
	cilk_spawn compute_wedge_y(A, B, x, uy, d);
	cilk_spawn compute_wedge_y(A, B, x, ly, d);

	cilk_sync;

	// compute reverse pyramid in the center
	cilk_spawn compute_reverse(A, B, x, y, d); cilk_sync;

	// compute tip
	cilk_spawn compute_pyramid(A, B, x, y, d); cilk_sync;

}


void compute_reverse(Grid* A, Grid* B, int x, int y, int s) {
	int l, i, j, d, h, ux, lx, uy, ly;
	assert(s % 2 == 1 && "Only odd sizes are supported!");

	// check for terminal case
	if (s <= CUT_OFF) {

		// compute height of pyramid
		int h = (s + 1) / 2;
		if (DEBUG) printf("Computing reverse pyramid at (%d,%d) with size %d  ...\n", x, y, s);

		// just compute the pyramid
		for(l=0; l<h; l++) {
			Grid* C;

			// compute one plain of the pyramid
			for(i = x-l; i<=x+l; i++) {
				for(j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			C = A;
			A = B;
			B = C;
		}

		// done
		return;
	}

	if (DEBUG) printf("Decomposing reverse pyramid at (%d,%d) with size %d ...\n", x, y, s);

	// cut into 6 smaller pyramids + 4 wedges

	// compute size of pyramids
	d = (s-1)/2;
	h = (d+1)/2;

	ux = x - h;
	lx = x + h;
	uy = y - h;
	ly = y + h;

	// compute tip
	cilk_spawn compute_reverse(A, B, x, y, d); cilk_sync;

	// compute reverse pyramid in the center
	cilk_spawn compute_pyramid(A, B, x, y, d); cilk_sync;

	// compute 4 wedges (parallel)
	cilk_spawn compute_wedge_y(A, B, ux, y, d);
	cilk_spawn compute_wedge_y(A, B, lx, y, d);
	cilk_spawn compute_wedge_x(A, B, x, uy,  d);
	cilk_spawn compute_wedge_x(A, B, x, ly, d);

	cilk_sync;

	// compute 4 base-pyramids (parallel)
	cilk_spawn compute_reverse(A, B, lx, ly,  d);
	cilk_spawn compute_reverse(A, B, lx, uy,  d);
	cilk_spawn compute_reverse(A, B, ux, ly,  d);
	cilk_spawn compute_reverse(A, B, ux, uy,  d);

	cilk_sync;

}


void compute_wedge_x(Grid* A, Grid* B, int x, int y, int s) {
	int l, i, j, d, h;
	assert(s > 0);

	if (s <= CUT_OFF) {

		// compute height of wedge
		int h = (s + 1) / 2;

		if (DEBUG) printf("Computing wedge %d/%d/%d/X - height %d ...\n", x, y, s, h);

		// just compute the wedge
		for(l=0; l<h; l++) {
			Grid* C;

			// compute one level of the wedge
			for(i = x-(h-l)+1; i<=x+(h-l)-1; i++) {
				for(j = y-l; j<=y+l; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			C = A;
			A = B;
			B = C;
		}

		return;
	}

	if (DEBUG) printf("Decomposing wedge %d/%d/%d/X ...\n", x, y, s);

	// decompose into 2 pyramids and 4 wedges

	// compute coordinates offset of sub-wedges
	d = (s-1)/2;
	h = (d+1)/2;

	// compute bottom wedges (parallel)
	cilk_spawn compute_wedge_x(A, B, x-h, y, d);
	cilk_spawn compute_wedge_x(A, B, x+h, y, d);
	cilk_sync;

	// reverse pyramid
	cilk_spawn compute_reverse(A, B, x, y, d); cilk_sync;

	// compute pyramid on top
	cilk_spawn compute_pyramid(A, B, x, y, d); cilk_sync;

	// compute remaining two wedges (parallel)
	cilk_spawn compute_wedge_x(A, B, x, y-h, d);
	cilk_spawn compute_wedge_x(A, B, x, y+h, d);
	cilk_sync;

}

void compute_wedge_y(Grid* A, Grid* B, int x, int y, int s) {
	int l, i, j, d, h;
	assert(s > 0);


	if (s <= CUT_OFF) {

		// compute height of wedge
		int h = (s + 1) / 2;

		if (DEBUG) printf("Computing wedge %d/%d/%d/Y ...\n", x, y, s);

		// just compute the wedge
		for(l=0; l<h; l++) {
			Grid* C;

			// compute one plain of the pyramid
			for(i = x-l; i<=x+l; i++) {
				//printf("Level %d - bounds x: %d,%d - bounds y: %d,%d\n", h, x-l, x+l, y-(h-l)+1, y+(h-l)-1 );
				for(j = y-(h-l)+1; j<=y+(h-l)-1; j++) {
					update(A,B,i,j);
				}
			}

			// switch plains
			C = A;
			A = B;
			B = C;
		}

		return;
	}

	if (DEBUG) printf("Decomposing wedge %d/%d/%d/Y ...\n", x, y, s);

	// decompose into 2 pyramids and 4 wedges

	// compute coordinates offset of sub-wedges
	d = (s-1)/2;
	h = (d+1)/2;

	// compute bottom wedges (parallel)
	cilk_spawn compute_wedge_y(A, B, x, y-h, d);
	cilk_spawn compute_wedge_y(A, B, x, y+h, d);
	cilk_sync;

	// reverse pyramid
	cilk_spawn compute_reverse(A, B, x, y, d); cilk_sync;

	// compute pyramid on top
	cilk_spawn compute_pyramid(A, B, x, y, d); cilk_sync;

	// compute remaining two wedges (parallel)
	cilk_spawn compute_wedge_y(A, B, x-h, y, d);
	cilk_spawn compute_wedge_y(A, B, x+h, y, d);
	cilk_sync;
}



void jacobi_recursive(Grid* A, Grid* B, int num_iter) {

	// compute full pyramid
	if (DEBUG) printf("\nProcessing main pyramid ...\n");
	cilk_spawn compute_pyramid(A, B, N/2, N/2, N-2); cilk_sync;
	if (DEBUG) printf("\nProcessing x wedge ...\n");
	cilk_spawn compute_wedge_x(A, B, N/2,0, N-2); cilk_sync;
	if (DEBUG) printf("\nProcessing y wedge ...\n");
	cilk_spawn compute_wedge_y(A, B, 0, N/2, N-2); cilk_sync;
	if (DEBUG) printf("\nProcessing reverse pyramid ...\n");
	cilk_spawn compute_reverse(A, B, 0, 0, N-2); cilk_sync;

}


int main() {
	double sum = 0.0;
	int ok = 1;

	double start, time;

	// allocate two copies of the processed array
	Grid* A = malloc(sizeof(Grid));
	Grid* B = malloc(sizeof(Grid));

	// fill input data into A
	int i, j;
	for(i=0; i<N; i++) {
		for (j=0; j<N; j++) {
			(*A)[i][j] = 0;
		}
	}

	// light a candle in the center
	(*A)[N/2][N/2] = 1;

	printf("Running problem size %d = %d x %d ...\n", P, N, M);

	// run computation
//	start = omp_get_wtime();
	printf("Using CUT = %d => CUT_OFF = %d\n", CUT, CUT_OFF);
	cilk_spawn jacobi_recursive(A,B,M); cilk_sync;
//	time = omp_get_wtime() - start;
//	printf("Execution time: %.1fms\n", time*1000);

	// verification
	for(i=0; i<N && ok; ++i) {
		for(j=0; j<N && ok; ++j) {
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
