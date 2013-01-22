#include <stdio.h>

int main(int argc, char **argv) {
	int v = 0;
	#pragma omp parallel num_threads(100)
	{
		for(int i=0; i<100; ++i) {
			#pragma omp atomic
			v += 1;
		}
	}
	printf("%d\n", v);
}