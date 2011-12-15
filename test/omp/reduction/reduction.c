#include <omp.h>
#include <stdio.h>

#define ITERATIONS 100000

int main() {
	double a = 0, b = 0;
	#pragma omp parallel
	{
		double c = 2;
		double d = 4;
		#pragma omp for reduction(+:a,b)
		for(int i=0; i<ITERATIONS; ++i) {
			a+=c;
			b+=d;
		}
		#pragma omp master
		{
			if(a != c * ITERATIONS) printf("FAIL a\n");
			else if(b != d * ITERATIONS) printf("FAIL b\n");
			else printf("SUCCESS\n");
		}
	}
	return 0;
}