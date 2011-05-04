
#include <stdio.h>


int min(int a, int b)  { return a<b?a:b; }

int main(int argc, char* argv[]) {

	int a = 10;
	int b = 20;

	int (*f)(int,int) = &min;

	printf("Direct call:  min(%d,%d)=%d\n", a, b, min(a,b));
	printf("Pointer call:   f(%d,%d)=%d\n", a, b, f(a,b));
}
