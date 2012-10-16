
#include <stdio.h>

#ifndef spawn
	#define spawn
#endif

#ifndef sync
	#define sync
#endif

int fib(int n) {
	if (n<2) return n;

	int x = spawn fib(n-1);
	int y;
	y = spawn fib(n-2);
	
	spawn fib(n-2);

	sync;
	return (x+y);
}

int main() {
	int N = 25;
	printf("fib(%d) = %d\n", N, fib(N));
	return 0;
}
