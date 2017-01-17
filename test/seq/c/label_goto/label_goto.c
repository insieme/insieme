#include <stdio.h>

int main() {
	printf("1\n");
	printf("2\n");
	goto four; //goto stmt
	three: //label stmt
	printf("3\n");
	return 0;
	four: //label stmt
	printf("4\n");
	printf("5\n");
	goto three; //goto stmt
	printf("6\n");
	return 0;
}

//1
//2
//4
//5
//3

