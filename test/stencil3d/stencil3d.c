#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef N
#define N 201
#endif

#ifndef K
#define K N
#endif

#ifndef M
#define M 1
#endif

#ifndef STENCIL_SIZE
#define STENCIL_SIZE 3
#endif
#define STENCIL_SIZE2 (STENCIL_SIZE/2)

#define INIT_VAL 100.0

double volume[2][N][N][K+1];

void print_volume(int buffer) {
	printf("\n---------------------------------------------\n");
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			for(int k=0; k<N; ++k) {
				printf("%12.4lf,", volume[buffer][i][j][k]);
			}
			printf("\n");
		}
		printf("\n--\n");
	}
}

int main() {
	if(N%2 == 0 || K%2 == 0) {
		printf("Size must be an odd number.\n");
		exit(-1);
	}
	if(STENCIL_SIZE%2 == 0 || STENCIL_SIZE*2 >= N) {
		printf("Stencil size must be an odd number and less than half of N.\n");
		exit(-1);
	}

	// initialize
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			for(int k=0; k<K; ++k) {
				volume[0][i][j][k] = (i == N/2 && j == N/2 && k == K/2) ? INIT_VAL : 0.0;
			}
		}
	}
	//print_volume(0);

	// stencil init
	double stencil[STENCIL_SIZE][STENCIL_SIZE][STENCIL_SIZE];
	{
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					double id = (i+0.5) - STENCIL_SIZE/2.0;
					double jd = (j+0.5) - STENCIL_SIZE/2.0;
					double kd = (k+0.5) - STENCIL_SIZE/2.0;
					stencil[i][j][k] = sqrt(id*id + jd*jd + kd*kd);
				}
			}
		}
		double stencil_base = stencil[0][0][0]+0.0001;
		double stencil_sum = 0.0;
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					stencil[i][j][k] = stencil_base - stencil[i][j][k];
					stencil_sum += stencil[i][j][k];
				}
			}
		}
		for(int i=0; i<STENCIL_SIZE; ++i) {
			for(int j=0; j<STENCIL_SIZE; ++j) {
				for(int k=0; k<STENCIL_SIZE; ++k) {
					stencil[i][j][k] /= stencil_sum;
					//printf("%8.6lf,", stencil[i][j][k]);
				}
				//printf("\n");
			}
			//printf("\n--\n");
		}
	}

	// calculation
	//#pragma omp parallel
	for(int iter=0; iter<M; iter++) {
		//#pragma omp for 
		//#pragma insieme tile(128,8)
		for(int i=STENCIL_SIZE2; i<N-STENCIL_SIZE2; ++i) {
			for(int j=STENCIL_SIZE2; j<N-STENCIL_SIZE2; ++j) {
				for(int k=STENCIL_SIZE2; k<K-STENCIL_SIZE2; ++k) {
					if(iter%2==0) {
						volume[1][i][j][k] = 0.0;
					} 
					if (iter%2 !=0) {
						volume[0][i][j][k] = 0.0;
					}
					for(int si=-STENCIL_SIZE2; si<=STENCIL_SIZE2; ++si) {
						for(int sj=-STENCIL_SIZE2; sj<=STENCIL_SIZE2; ++sj) {
							for(int sk=-STENCIL_SIZE2; sk<=STENCIL_SIZE2; ++sk) {
								if(iter%2==0) {
									volume[1][i][j][k] += volume[0][i+si][j+sj][k+sk] * stencil[si+STENCIL_SIZE2][sj+STENCIL_SIZE2][sk+STENCIL_SIZE2];
								} 
								if (iter%2!=0) {
									volume[0][i][j][k] += volume[1][i+si][j+sj][k+sk] * stencil[si+STENCIL_SIZE2][sj+STENCIL_SIZE2][sk+STENCIL_SIZE2];
								}
							}
						}
					}
				}
			}
		}
		//#pragma omp master
		//print_volume(1-(iter%2));
	}

	// verification
	double sum = 0.0;
	int ok = 1, final = 1-(M-1)%2;
	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			for(int k=0; k<K; ++k) {
				sum += volume[final][i][j][k];
				if(labs(N/2-i) > M*STENCIL_SIZE2) {
					if(volume[final][i][j][k] != 0.0) { ok = 0; printf("FAIL B %d/%d/%d\n", i,j,k); exit(-1); }
				}
			}
		}
	}
	if(abs(sum-INIT_VAL) > 0.00001) { ok = 0; printf("FAIL SUM %lf\n", abs(sum-INIT_VAL)); exit(-1); }

	printf("Verification: %s\n", ok?"OK":"FAILED");

	return !ok;
}
