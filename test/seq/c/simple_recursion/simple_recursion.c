
#include <stdio.h>

int rec_num=0;

int fib(int n) {
	if (n==0) { return 0; }
	if (n==1) { return 1; }

	// use globals
	++rec_num; 

	return fib(n-1) + fib(n-2);
}


int main (int argc, char* argv[]) {

	rec_num=0;
	printf("%d) fib(0)=%d\n", rec_num, fib(0));

	rec_num=0;
	printf("%d) fib(1)=%d\n", rec_num, fib(1));

	rec_num=0;
	printf("%d) fib(3)=%d\n", rec_num, fib(3));

	rec_num=0;
	printf("%d) fib(5)=%d\n", rec_num, fib(5));

	rec_num=0;
	printf("%d) fib(7)=%d\n", rec_num, fib(7));

	rec_num=0;
	printf("%d) fib(10)=%d\n", rec_num, fib(10));
}
