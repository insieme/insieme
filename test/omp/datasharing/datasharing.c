#include <stdio.h>

int main() {
	int nt = 0;
	int s = 0;
	int m = 0;
	#pragma omp parallel shared(m,nt) firstprivate(s)
	{
		for(int i=0; i<10000; ++i)
			s++;
		#pragma omp critical
		m += s;
		#pragma omp master
		nt = omp_get_num_threads();
	}

	if (m == nt * 10000) {
		printf("Success!\n");
		return 0;
	} else {
		printf("Fail!\n");
		return 1;
	}
}
