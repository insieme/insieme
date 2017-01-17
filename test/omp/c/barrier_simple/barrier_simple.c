#include <stdio.h>


int main() {
	int i = 0;
	int sum = 0;

 	// sure, it isn't nice, but ...
	#pragma omp parallel 
	{
		for (int k=0; k<1000; ++k) {
			int m = 0;
			i=1;
			#pragma omp barrier
			m = i;
			#pragma omp barrier
			i=0;
			
			#pragma omp master
			sum += m;
		}
	}

	// check whether sum works out ...
	if (sum == 1000) {
		printf("Success!\n");
	} else {
		printf("Fail! - %d\n", sum);
	}
}
