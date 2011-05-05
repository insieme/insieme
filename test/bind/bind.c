//
// A test case aiming on producing a bind node and a corresponding call.
//

#include<stdio.h>

#define min(a,b) (((a) < (b)) ? (a) : (b))

int main() {

	int a = 10;
	int b = 20;
	int c = 5;

	// this is causing the creation of a bind node (min is a macro)
	int m = min(a,min(b,c));

	printf("Min: %d\n", m);
}
