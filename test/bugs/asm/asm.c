
#include <stdio.h>


int foo () {
	int x = 42;
	int *y = &x;
	int r;
	asm (""
		: "=&d" (r) : "a" (y), "m" (*y));
	return r;
}

int main (){

	int x = foo();

	return x == 42;
}
