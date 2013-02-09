
#include <stdio.h>
#include <stddef.h>

#define N 50

int main() {


	int a[N];

	int* s = a;
	int* e = s + N;

	#pragma omp parallel for
	for(int* p = s; p < e; p++) {
		*p = 2;
	}

	int sum = 0;
	#pragma omp parallel for reduction(+ : sum)
	for(int* p = s; p < e; p++) {
		sum += *p;
	}

	sum = 0;
	#pragma omp parallel for reduction(+ : sum)
	for(ptrdiff_t i = 0; i < (e-s); ++i) {
		sum += s[i];
	}

	printf("sum=%d\n", sum);
	return (sum == N*2)?0:1;
}

