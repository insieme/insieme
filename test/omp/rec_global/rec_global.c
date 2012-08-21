#include <omp.h>

int a(int x) {
	int y;
	#pragma omp critical
	y = x - 1;
	if(y<0) return 0;
	return a(x-1) + 1;
}


int main(int argc, char** argv) {
	#pragma omp parallel
	{
		#pragma omp master
		printf("%d - %d\n", omp_get_thread_num(), a(omp_get_thread_num()));
	}
}