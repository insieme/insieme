
#include <stdio.h>
#include <cilk/cilk.h>

int fib(int n) {
	if (n<2) return n;

	int x = cilk_spawn fib(n-1);
	int y;
	y = cilk_spawn fib(n-2);
	
	cilk_spawn fib(n-2);

	cilk_sync;
	return (x+y);
}

int main() {
	int N = 15;
	printf("fib(%d) = %d\n", N, fib(N));
	return 0;
}
