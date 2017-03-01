#include <stdio.h>

int main(int argc, char** arv) {
	int si = 0, sj = 1, i, n, nseqs = 1000;
	
	unsigned long long sum = 0;
	
	#pragma omp parallel
	{
		#pragma omp single private(si,sj,i,n)
		for (si = 0; si < nseqs; si++) {
			i = 1;
			n = 10;
			for (sj = si + 1; sj < nseqs; sj++) {
				#pragma omp task private(i) firstprivate(n) shared(sum) untied
				{
					for(i=5; i<n; ++i) {
						#pragma omp atomic
						sum += i;
					}
				}
			}
		}
	}
	printf("%llu\n", sum);
}