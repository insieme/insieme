#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

typedef unsigned long dn;

dn delannoy(dn x, dn y) {
	if(x==0 || y==0) return 1;

	dn a=0, b=0, c=0;
	#pragma omp task untied shared(a)
	a = delannoy(x-1, y);
	#pragma omp task untied shared(b)
	b = delannoy(x-1, y-1);
	#pragma omp task untied shared(c)
	c = delannoy(x, y-1);

	#pragma omp taskwait
	return a+b+c;
}

dn DELANNOY_RESULTS[] = {
	1, 3, 13, 63, 321, 1683, 8989, 48639, 265729, 1462563, 8097453, 45046719, 251595969, 1409933619, 
	7923848253, 44642381823, 252055236609, 1425834724419, 8079317057869, 45849429914943, 260543813797441, 
	1482376214227923, 8443414161166173};

int NUM_RESULTS = sizeof(DELANNOY_RESULTS) / sizeof(dn);

int main(int argc, char **argv) {
	if(argc<2) {
		printf("Usage: delannoy N [+t]\n");
		exit(-1);
	}

	int n = atoi(argv[1]);
	if(n >= NUM_RESULTS) {
		printf("N too large (can only check up to %d)\n", NUM_RESULTS);
		exit(-1);
	}

#ifdef _OPENMP
	double start = omp_get_wtime();
#endif
	dn result = 0;
	#pragma omp parallel
	{
		#pragma omp single
		{
			result = delannoy(n, n);
		}
	}
#ifdef _OPENMP
	double time = omp_get_wtime() - start;
	if(argc > 2 && strcmp(argv[2],"+t") == 0) printf("Time Program        = %.6f seconds\n", time);
#endif

	if(result == DELANNOY_RESULTS[n]) printf("Verification: SUCCESS\n");
	else printf("Verification: FAILED\n");
}
