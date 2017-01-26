#include <stdio.h>

int calculate(int a) { return a; }
double calculate(double a) { return a; }

int calcDefArg(int a, int b=10, int c=100) { return a+b+c; };

int main() {
	// test function overloading
	printf("1 == %d\n", calculate(1)); 		// int
	printf("2.2 == %f\n", calculate(2.2));	// double

	// test default arguments
	printf("100+0+0 == %d\n", calcDefArg(100,0,0));		//=100
	printf("1+100+100 == %d\n", calcDefArg(1,100));		//=201
	printf("100+10+100 == %d\n", calcDefArg(100));		//=210

	return 0;
}
