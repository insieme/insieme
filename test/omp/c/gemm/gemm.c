
#include <stdio.h>
#include <math.h>
#include <omp.h>

// -------- problem size ------------

#ifndef N
	#define N (256)
#endif

// -------- to support cilk in gcc ------------

#ifndef cilk
	#define cilk
#endif
#ifndef spawn
	#define spawn
#endif
#ifndef sync
	#define sync
#endif

// -------- some utilities ------------

#define bool int
#define true 1
#define false 0

#define value double
#define eps 0.0000001

value A[N][N];
value B[N][N];
value C[N][N];


void reset();
bool check();

void mm_naive();
void mm_omp_naive();

void mm_tiled();
void mm_omp_tiled();

void mm_recursive();
void mm_omp_recursive();
void mm_cilk_recursive();

#define MIN(X,Y) (((X)<(Y))?(X):(Y))
#define MAX(X,Y) (((X)>(Y))?(X):(Y))

double median(double a, double b, double c) {
	return MIN(MAX(a,b),MAX(b,c));
}


int CUT = 20;
void calibrate_cut_off();


typedef struct { int i; int j; int k; } tile_sizes;

tile_sizes TILE = { 10, 10, 10 };
void calibrate_tile_size();

#define RUN_TEST(IMPL,NAME) \
{ \
	reset(); \
	/*double time = omp_get_wtime();*/ \
	IMPL(); \
	/*time = omp_get_wtime() - time;*/ \
	/*printf("%s: time = %fsec, success: %s\n", NAME, time, (check())?"true":"false");*/ \
	printf("%s: success: %s\n", NAME, (check())?"true":"false"); \
}


int main() {

	printf("Problem Size: %d\n", N);

	// calibrate_tile_size();
	TILE = (tile_sizes){64, 256, 32}; // priviously determined tile size

	//calibrate_cut_off();
	CUT = 256; // priviously determined cut-off


	RUN_TEST(mm_naive,     	    "naive");
	RUN_TEST(mm_omp_naive,      "naipl");
	RUN_TEST(mm_tiled,          "tiled");
	RUN_TEST(mm_omp_tiled,      "tilpl");
	RUN_TEST(mm_recursive, 	    "recur");
	RUN_TEST(mm_cilk_recursive, "recpl");


	// check
	if (!check()) {
		printf("Verification failed!\n");
		return 1;
	}

	printf("Successfull\n");
	return 0;
}



void reset() {
	// fill data structure
	#pragma omp parallel for
	for(int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			A[i][j] = i * j;
			B[i][j] = (i==j) ? 1 : 0 ;
			C[i][j] = 0;
		}
	}
}

bool check() {
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			if (fabs(C[i][j] - i*j) > eps) {
				printf("C[%d][%d]=%f!=%f\n", i, j, C[i][j], (double)i*j);
				return false;
			}
		}
	}
	return true;
}

void mm_naive() {
	for(int i=0; i<N; i++) {
		for(int k=0; k<N; k++) {
			for(int j=0; j<N; j++) {
				C[i][j] += A[i][k] * B[k][j];
			}	
		}
	}
}

void mm_omp_naive() {
	#pragma omp parallel for
	for(int i=0; i<N; i++) {
		for(int k=0; k<N; k++) {
			for(int j=0; j<N; j++) {
				C[i][j] += A[i][k] * B[k][j];
			}	
		}
	}
}

void mm_tiled() {
	for(int ii=0; ii<N; ii+=TILE.i) {
		int iu = MIN(N,ii+TILE.i);
		for(int jj=0; jj<N; jj+=TILE.j) {
			int ju = MIN(N,jj+TILE.j);
			for(int kk=0; kk<N; kk+=TILE.k) {
				int ku = MIN(N,kk+TILE.k);

				for(int i=ii; i<iu; i++) {
					for(int k=kk; k<ku; k++) {
						for(int j=jj; j<ju; j++) {
							C[i][j] += A[i][k] * B[k][j];
						}	
					}
				}


			}	
		}
	}
}

void mm_omp_tiled() {
	#pragma omp parallel for
	for(int ii=0; ii<N; ii+=TILE.i) {
		int iu = MIN(N,ii+TILE.i);
		for(int jj=0; jj<N; jj+=TILE.j) {
			int ju = MIN(N,jj+TILE.j);
			for(int kk=0; kk<N; kk+=TILE.k) {
				int ku = MIN(N,kk+TILE.k);

				for(int i=ii; i<iu; i++) {
					for(int k=kk; k<ku; k++) {
						for(int j=jj; j<ju; j++) {
							C[i][j] += A[i][k] * B[k][j];
						}	
					}
				}


			}	
		}
	}
}



void calibrate_tile_size() {
	printf("Starting Tile-Size Calibration ...\n");
	tile_sizes best; //double bestTime = 10000;

	// N= 256 => 64/256/32
	// N= 512 => 32/256/32
	// N=1024 => 64/256/32

	for(int i=4; i<=128; i+=4) {
	for(int j=200; j<=300; j+=4) {
	for(int k=4; k<=128; k+=4) {
		reset();
		TILE.i = i;
		TILE.j = j;
		TILE.k = k;
		//double times[3];
		printf("\tTile-Size: %d/%d/%d\n", TILE.i, TILE.j, TILE.k);
		for (int j=0; j<3; j++) {
			//times[j] = omp_get_wtime();
			mm_tiled();
			//times[j] = omp_get_wtime() - times[j];
			//printf("\t\tTotal time: %fsec\n", times[j]);
		}
		//double time = median(times[0], times[1], times[2]);
		//if (time < bestTime) {
		//	best = TILE;
		//	bestTime = time;
		//}
	}
	}
	}
	//printf("Best tile size: %d/%d/%d - %f\n", best.i, best.j, best.k, bestTime);
	printf("Best tile size: %d/%d/%d\n", best.i, best.j, best.k);
	TILE = best;
}



void calibrate_cut_off() {
	printf("Starting Calibration ...\n");
	int best = 0; //double bestTime = 10000;
	for(int i=8; i<=256; i=i<<1) {
		reset();
		CUT = i;
		//double times[3];
		printf("\tCut-off: %d\n", CUT);
		for (int j=0; j<3; j++) {
			//times[j] = omp_get_wtime();
			mm_recursive();
			//times[j] = omp_get_wtime() - times[j];
			//printf("\t\tTotal time: %fsec\n", times[j]);
		}
		//double time = median(times[0], times[1], times[2]);
		//if (time < bestTime) {
		//	best = i;
		//	bestTime = time;
		//}
	}
	//printf("Best CUT: %d - %f\n", best, bestTime);
	printf("Best CUT: %d\n", best);
	CUT = best;
}


void mm_recursive_helper(
	int ax, int ay, 	// matrix A
	int bx, int by, 	// matrix B
	int cx, int cy, 	// matrix C
	int n 			// size
) {

	// base case
	if (n < CUT) {

		for(int i=0; i<n; i++) {
			for(int k=0; k<n; k++) {
				for(int j=0; j<n; j++) {
					C[cx+i][cy+j] += A[ax+i][ay+k] * B[bx+k][by+j];
				}
			}
		}			
/*
		// if tiling is supported, CUT => max, since tiling is fucking fast

		// standard computation using tiling ..
		for(int ii=0; ii<n; ii+=TILE.i) {
			int iu = MIN(ii+TILE.i, n);
		for(int jj=0; jj<n; jj+=TILE.j) {
			int ju = MIN(jj+TILE.j, n);
		for(int kk=0; kk<n; kk+=TILE.k) {
			int ku = MIN(kk+TILE.k, n);

		for(int i=ii; i<iu; i++) {
			for(int k=kk; k<ku; k++) {
				for(int j=jj; j<ju; j++) {
					C[cx+i][cy+j] += A[ax+i][ay+k] * B[bx+k][by+j];
				}	
			}
		}

		}}} // end of tiling
*/
		return;
	}	

	// recursive decomposition
	int n2 = n/2;

	// compute 8 sub-problems
	
	// C11
	mm_recursive_helper(ax,    ay, bx, by,    cx, cy, n2);
	mm_recursive_helper(ax+n2, ay, bx, by+n2, cx, cy, n2);

	// C12
	mm_recursive_helper(ax,    ay, bx+n2, by,    cx+n2, cy, n2);
	mm_recursive_helper(ax+n2, ay, bx+n2, by+n2, cx+n2, cy, n2);

	// C21
	mm_recursive_helper(ax,    ay+n2, bx, by,    cx, cy+n2, n2);
	mm_recursive_helper(ax+n2, ay+n2, bx, by+n2, cx, cy+n2, n2);

	// C22
	mm_recursive_helper(ax,    ay+n2, bx+n2, by,    cx+n2, cy+n2, n2);
	mm_recursive_helper(ax+n2, ay+n2, bx+n2, by+n2, cx+n2, cy+n2, n2);

}


void mm_recursive() {

	mm_recursive_helper(0,0,0,0,0,0,N);

}

void mm_cilk_recursive_helper(
	int ax, int ay, 	// matrix A
	int bx, int by, 	// matrix B
	int cx, int cy, 	// matrix C
	int n 			// size
) {

	// base case
	if (n < CUT) {

		for(int i=0; i<n; i++) {
			for(int k=0; k<n; k++) {
				for(int j=0; j<n; j++) {
					C[cx+i][cy+j] += A[ax+i][ay+k] * B[bx+k][by+j];
				}
			}
		}
/*
		// standard computation using tiling ..
		for(int ii=0; ii<n; ii+=TILE.i) {
			int iu = MIN(ii+TILE.i, n);
		for(int jj=0; jj<n; jj+=TILE.j) {
			int ju = MIN(jj+TILE.j, n);
		for(int kk=0; kk<n; kk+=TILE.k) {
			int ku = MIN(kk+TILE.k, n);

		for(int i=ii; i<iu; i++) {
			for(int k=kk; k<ku; k++) {
				for(int j=jj; j<ju; j++) {
					C[cx+i][cy+j] += A[ax+i][ay+k] * B[bx+k][by+j];
				}	
			}
		}

		}}} // end of tiling
*/
		return;
	}	

	// recursive decomposition
	int n2 = n/2;

	// compute 8 sub-problems
	
	// C11
	#pragma omp task
	{
		mm_recursive_helper(ax,    ay, bx, by,    cx, cy, n2);
		mm_recursive_helper(ax+n2, ay, bx, by+n2, cx, cy, n2);
	}

	// C12
	#pragma omp task
	{
		mm_recursive_helper(ax,    ay, bx+n2, by,    cx+n2, cy, n2);
		mm_recursive_helper(ax+n2, ay, bx+n2, by+n2, cx+n2, cy, n2);
	}

	// C21
	#pragma omp task
	{
		mm_recursive_helper(ax,    ay+n2, bx, by,    cx, cy+n2, n2);
		mm_recursive_helper(ax+n2, ay+n2, bx, by+n2, cx, cy+n2, n2);
	}

	// C22
	#pragma omp task
	{
		mm_recursive_helper(ax,    ay+n2, bx+n2, by,    cx+n2, cy+n2, n2);
		mm_recursive_helper(ax+n2, ay+n2, bx+n2, by+n2, cx+n2, cy+n2, n2);
	}

	#pragma omp taskwait

}


void mm_cilk_recursive() {

	#pragma omp parallel
	{
		#pragma omp single
		mm_cilk_recursive_helper(0,0,0,0,0,0,N);
	}

}

