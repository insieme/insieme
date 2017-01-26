
#include <stdio.h>
#include <omp.h>

int main() {

	omp_set_num_threads(5);
	#pragma omp parallel
	{
		#pragma omp master
		printf("Num (5) : %d", omp_get_num_threads());
	}
	
	omp_set_num_threads(2);
	#pragma omp parallel
	{
		#pragma omp master
		printf("Num (2) : %d", omp_get_num_threads());
	}
	
	return 0;
}