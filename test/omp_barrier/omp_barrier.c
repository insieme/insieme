#include <stdio.h>

int main() {
  int i;
	#pragma omp parallel
	{
    i++;
    #pragma omp barrier
		printf("i: %d\n", i);
	}
}
