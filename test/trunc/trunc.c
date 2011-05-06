#include <stdio.h>


int main() {

	double x, y;

	x = 12.123;
	y = x;	
	printf("Before: %f\n", y);

	y = (int) x;
	printf("After: %f\n", y);
}
