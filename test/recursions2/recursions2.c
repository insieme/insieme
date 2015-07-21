#include <stdio.h>

// extern int printf(char *, ...);


int three (int a, int b);
int two  ( int a, int b);
int one  (int a, int b);

int three (int a, int b){

	if (a == 0)
		return b;
	else
		return one(a,b);
}
int two  ( int a, int b){
	return three (a-1, b);
}
int one  (int a, int b){
	return two (a, b*2);
}


int recursion2 (int *v, int size){
	if (size > 0){
		printf("%d=%d\n",v[size-1], one(10,v[size-1]));
		return recursion2(v, size-1);
	}
	return 0;
}

unsigned fac(unsigned x) {
	if (x>1) {
		return x * fac(x-1);
	} else {
		return 1;
	}
}

int main(int argc, char* argv[]) {

	int list[] = {12,45,6,7,85};
	int x = 10;
	printf("x=%d\n", one(10, x));
	printf("x=%d\n", fac(10));

	recursion2(list, 5);

	printf("x=%d\n", fac(10));
	printf("x=%d\n", one(10, x));
	printf("x=%d\n", fac(10));


	return 0;
}
