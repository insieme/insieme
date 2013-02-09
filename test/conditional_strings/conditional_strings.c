#include <stdio.h>

int main() {

	int a = 0;

	printf("a==0 is %s\n", (a==0)?"true":"false");
	printf("a==1 is %s\n", (a==1)?"true":"false");

	printf("a==1 is %s\n", (a==1)?"si":"no");
	printf("a==1 is %s\n", (a==1)?"+":"-");
	return 0;
}
