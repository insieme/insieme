#include <stdio.h>

int main() {

	{ 
		int a[20] = { 1 };
		printf("a[0]=%d\n", a[0]);
	}
	{
		// partially initialized integer array
		int a[20] = { 1, 2 };
		printf("a[0]=%d\n", a[0]);
		printf("a[1]=%d\n", a[1]);
	}
	{
		// fully initialized integer array
		int a[3] = { 1, 2, 3 };
		printf("a[0]=%d\n", a[0]);
		printf("a[1]=%d\n", a[1]);
		printf("a[2]=%d\n", a[2]);
	}
	{
		// array with undefined size
		int a[] = { 1, 2, 3, 4 };
		printf("a[0]=%d\n", a[0]);
		printf("a[1]=%d\n", a[1]);
		printf("a[2]=%d\n", a[2]);
		printf("a[3]=%d\n", a[3]);
	}
	{
		// string literals
		char s[20] = "a";
		printf("s[0]=%c\n", s[0]);
		printf("s[1]=%c\n", s[1]);
	}
	{
		// string literals
		char s[20] = "abc";
		printf("s[0]=%c\n", s[0]);
		printf("s[1]=%c\n", s[1]);
		printf("s[2]=%c\n", s[2]);
		printf("s[3]=%c\n", s[3]);
	}
	return 0;
}

