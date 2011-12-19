#include <stdio.h>
#include <omp.h>

int flag;

int main() {
	#pragma omp parallel
	{
		if(omp_get_thread_num() == 0) flag = 1;
		while(flag == 0) {
			#pragma omp flush(flag)
		}
	}
	// successful if we reach this point at all
	printf("Success!\n");
}
