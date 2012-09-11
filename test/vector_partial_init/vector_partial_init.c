#include <stdio.h>

int main() {

	// partially initialized integer array
	int a[20] = { 1, 2 };
	printf("a[0]=%d\n", a[0]);
	printf("a[1]=%d\n", a[1]);

	// fully initialized integer array
	int b[3] = { 1, 2, 3 };
	printf("b[0]=%d\n", b[0]);
	printf("b[1]=%d\n", b[1]);
	printf("b[2]=%d\n", b[2]);

	// array with undefined size
	int c[] = { 1, 2, 3, 4 };
	printf("c[0]=%d\n", c[0]);
	printf("c[1]=%d\n", c[1]);
	printf("c[2]=%d\n", c[2]);
	printf("c[3]=%d\n", c[3]);

	// string literals
	char s[20] = "a";
	printf("s[0]=%c\n", s[0]);

	return 0;
}

