#include <stdio.h>
#include <omp.h>

int flag[2];

int main() {
	#pragma omp parallel
	{
		if(omp_get_thread_num() == 0) flag[0] = 1;
		while(flag[0] == 0) {
			#pragma omp flush(flag)
		}
	}
	// successful if we reach this point at all
	printf("Success!\n");
}
