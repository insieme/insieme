#include <omp.h>


int main() {
	#pragma omp parallel for
	for(int i=0; i<10; i++) {
		i--;
		#pragma omp barrier
	}
	return 0;
}
