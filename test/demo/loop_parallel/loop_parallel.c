
#include <stdio.h>

int sum(int x) {
	int res = 0;
	#pragma omp parallel for reduction(+:res)
	for(int i=1; i<=x; i++) {
		res += i;
	}
	return res;
}

int fac(int x) {
	int res = 1;
	#pragma omp parallel for reduction(*:res)
	for(int i=1; i<=x; i++) {
		res *= i;
	}
	return res;
}

int main() {

	int n = 5;	

	printf("%d!=%d\n", n, fac(n));
	printf("sum(1..%d) = %d\n", n, sum(n));

	return 0;
}
