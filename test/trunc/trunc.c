#include <stdio.h>


int main() {

	double x, y;

	x = 12.123;
	y = x;	
	printf("Before: %f\n", y);

	y = (int) x;
	printf("After: %f\n", y);

	int k = 2;
	printf("Double: %f\n", k*x);

}
