#include <stdio.h>

int main(int argc, char** argv) {

	int j, cnt = 0;
	int v[15] = {0};
	int vec[15] = {0};
  	int iterations = 0;
	int x;
	int i;

	int a;
	for(a=1;a<15;a+=3) {
		printf("13: a=%d\n", a);
	}

	for(i = 8; i>=0; --i) {
		x = i;
		iterations++;
		printf("x: %d\n", x);
	}
	printf("Iterations: %d\n", iterations);
	printf("x: %d\n", x);
	printf("i: %d\n", i);
	{
	for(unsigned char i = 8, j = 7; i>0; --i) {
		printf("j: %d\n", j);
		iterations++;
	}
	printf("Iterations: %d\n", iterations);

	//CAREFULL condition: -1 is int, i gets casted to int so it holds
	//but in increment --i operates as char (underflow at 0 restart at 255)
	//--> infinite loop as value of i in coditionExpr is always between 0 nad 255 which is greater than -1
	//for(unsigned char i = 8, j = 7; i>=-1; --i) {
	
	//BUG: gcc evaluates conditionExpr as false
	//as i==8 (as char) but promotes i and -1 to unsigned int which results in comparison of 8>=UINT_MAX
	for(unsigned int i = 8, j = 7; i>=-1; --i) {
		printf("j: %d\n", j);
		iterations++;
	}
	printf("Iterations: %d\n", iterations);

	}
	printf("Iterations: %d\n", iterations);

	for(; i < 10; i+=1) {
		iterations++;
	}

	printf("Iterations: %d\n", iterations);
	
	vec[10] = 15;
	for(j=0; j<vec[10]; j++)
	{
		cnt ++;
		if(j == 10)
			vec[10] ++;
	}
	printf("Iterations: %d\n", cnt);

	int* ptr1;
	int* ptr2 = &vec[14];

	for(ptr1=vec; ptr1!=ptr2; ptr1++) {
		printf("%d ", *ptr1);
		cnt++;
	}

	printf("Iterations: %d\n", cnt);
	return 0;
}
