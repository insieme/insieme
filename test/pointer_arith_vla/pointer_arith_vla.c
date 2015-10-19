#include <stddef.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
	{
		int n=4, m=3;
		int a[n][m];
		int (*p)[m] = a;	// p == &a[0]
		printf(" %d \n", p == &a[0]);

		p+=1;				// p == &a[1]
		printf(" %d \n", p == &a[1]);

		(*p)[2] = 99;		// a[1][2] == 99
		printf("%d == 99\n", a[1][2]);

		n = p - a;			// n == 1

		printf(" %d == 1\n", n);
		printf(" %d\n", n == 1);
	}
}
