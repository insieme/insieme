#include <stdio.h>

int main() {
	int s = 0;
	int m = 0;
	#pragma omp parallel shared(m) private(s)
	{
		#pragma omp critical
		//todo
	}

	if (m > 0 && s == 1) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}
}
