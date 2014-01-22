
#include <stdio.h>

int iterations;

void loop (int min, int max, int step){
		printf( " loop: %d %d %d \n", min, max, step);
	for (int i = min; i < max; i+=step){
		iterations++;
		printf( " iteration: %d %d \n", i, iterations);
	}
}

void loopEQ (int min, int max, int step){
		printf( " loop: %d %d %d \n", min, max, step);
	for (int i = min; i <= max; i+=step){
		iterations++;
		printf( " iteration: %d %d \n", i, iterations);
	}
}

void loopREV (int min, int max, int step){
		printf( " loop: %d %d %d \n", min, max, step);
	for (int i = min; i > max; i+=step){
		iterations++;
		printf( " iteration: %d %d \n", i, iterations);
	}
}

int main (){

	iterations = 0;

	{
  	loop (0,10,1);
	loop (0,10,2);
	loop (0,10,3);

	loop (2,10,1);
	loop (2,10,2);
	loop (2,10,3);

	loop (5,10,1);
	loop (5,10,2);
	loop (5,10,3);

	loop (7,10,1);
	loop (7,10,2);
	loop (7,10,3);

	loop (-10, 0, 1);
	loop (-10, 0, 2);
	loop (-10, 0, 3);

	loop (10, 0, -1);
	loop (10, 0, -2);
	loop (10, 0, -3);
	}

	{
	loopEQ (0,10,1);
	loopEQ (0,10,2);
	loopEQ (0,10,3);

	loopEQ (2,10,1);
	loopEQ (2,10,2);
	loopEQ (2,10,3);

	loopEQ (5,10,1);
	loopEQ (5,10,2);
	loopEQ (5,10,3);

	loopEQ (7,10,1);
	loopEQ (7,10,2);
	loopEQ (7,10,3);

	loopEQ (-10, 0, 1);
	loopEQ (-10, 0, 2);
	loopEQ (-10, 0, 3);

	loopEQ (10, 0, -1);
	loopEQ (10, 0, -2);
	loopEQ (10, 0, -3);
	}

	{
	loopREV (0,10,1);
	loopREV (0,10,2);
	loopREV (0,10,3);

	loopREV (2,10,1);
	loopREV (2,10,2);
	loopREV (2,10,3);

	loopREV (5,10,1);
	loopREV (5,10,2);
	loopREV (5,10,3);

	loopREV (7,10,1);
	loopREV (7,10,2);
	loopREV (7,10,3);

	loopREV (-10, 0, 1);
	loopREV (-10, 0, 2);
	loopREV (-10, 0, 3);

	loopREV (10, 0, -1);
	loopREV (10, 0, -2);
	loopREV (10, 0, -3);
	}

	//example of a loop using pointers
	const char* str = "test";
	for (char* p = &str[0]; *p != '\0'; ++p) {
		printf("%c\n",*p);
	}

	//example of a loop using double as iterator -- drop to while
	for (double d = 0; d<10; d++) {
		printf("%f\n",d);
	}
	return 0;
}
