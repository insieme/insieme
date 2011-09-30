#include <stdio.h>

int main() {
	int s = 0;
	int m = 0;
	#pragma omp parallel
	{
		m++;
		//printf("multiple\n");
		#pragma omp single
		{
			s++;
			printf("single\n");
		}
	}

	if (m > 0 && s == 1) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}
}
