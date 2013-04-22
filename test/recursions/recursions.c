#include <stdio.h>

//typedef int bool;
#define bool int

#define true 1
#define false 0

// example - simple recursive function
unsigned fac(unsigned x) {
	if (x>1) {
		return x * fac(x-1);
	} else {
		return 1;
	}
}

// example - mutually recursive function
bool even(unsigned x);
bool odd(unsigned x);

bool even(unsigned x) {
	if (x==0) {
		return true;
	} else {
		return odd(x-1);
	}
}

bool odd(unsigned x) {
	if (x==0) {
		return false;
	} else {
		return even(x-1);
	}
}

// example - nested recursive function
unsigned ack(unsigned n, unsigned m) {
	if (n == 0) return m + 1;
	if (m == 0) return ack(n-1, 1);
	return ack(n-1, ack(n, m-1));
}

char* toStr(bool value) {
	if (value) return "true";
	return "false";
}


// using the given functions
int main(int argc, char* argv[]) {
	int x = 10;
	printf("x=%d\n", x);
	printf("fac(x)=%d\n", fac(x));
	printf("fac(x+1)=%d\n", fac(x+1));
	printf("even(x)=%s\n", toStr(even(x)));
	printf("odd(x)=%s\n", toStr(odd(x)));
	printf("ack(1,x)=%d\n", ack(1,x));
	printf("ack(2,x)=%d\n", ack(2,x));
	return 0;
}
