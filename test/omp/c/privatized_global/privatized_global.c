#include <stdio.h>

int x;

#define N 10


void check(int y, int z) {
	if(y != z) {
		printf("y: %d, expected: %d\n", y, z);
		printf("FAIL\n");
		exit(-1);
	};
}

int main(int argc, char** argv) {
	x = 5;
	#pragma omp parallel firstprivate(x)
	{
		check(x, 5);
		#pragma omp for
		for (int i = 0; i<omp_get_num_threads(); ++i) {
			for (int si = 0; si < N; si++) {
				x++;
			}
		}
		check(x, N+5);
		
	}
	check(x, 5);
	printf("Sucess\n");
}