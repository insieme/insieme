#include <omp.h>

int a(int x) {
	#pragma omp critical
	if(x==0) return 0;
	return a(x-1) + 1;
}


int main(int argc, char** argv) {
	#pragma omp parallel
	{
		#pragma omp master
		printf("%d - %d\n", omp_get_thread_num(), a(omp_get_thread_num()));
	}
}