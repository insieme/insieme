
#include <stdio.h>

int main() {

	int n = 5;
	int res;

	res = 12;
	#pragma omp parallel for reduction(+:res)
	for(int i=1; i<=n; i++) {
		res += i;
	}
	printf("+: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(-:res)
	for(int i=1; i<=n; i++) {
		res -= i;
	}
	printf("-: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(*:res)
	for(int i=1; i<=n; i++) {
		res *= i;
	}
	printf("*: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(&:res)
	for(int i=1; i<=n; i++) {
		res = res & i;
	}
	printf("&: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(|:res)
	for(int i=1; i<=n; i++) {
		res = res | i;
	}
	printf("|: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(^:res)
	for(int i=1; i<=n; i++) {
		res = res ^ i;
	}
	printf("^: %d\n", res);

/*
	res = 12;
	#pragma omp parallel for reduction(&&:res)
	for(int i=1; i<=n; i++) {
		res = res && i;
	}
	printf("&&: %d\n", res);


	res = 12;
	#pragma omp parallel for reduction(||:res)
	for(int i=1; i<=n; i++) {
		res = res || i;
	}
	printf("||: %d\n", res);
*/
}
