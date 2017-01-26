#include <stdio.h>

int x;

int *fn()
{
	printf("I got called!\n");
	x++;
	return &x;
}

int main(void)
{
	x = 1;
	*(fn()) += 2;
	printf("x: %d\n", x);

	int i = 1;
	int arr[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	arr[i++] += 1; // 0, 2, 2, 3, 4, ...
	printf("arr: [ %d %d %d %d %d ... ]\n", arr[0], arr[1], arr[2], arr[3], arr[4]);
	arr[++i] += 1; // 0, 2, 2, 4, 4, ...
	printf("arr: [ %d %d %d %d %d ... ]\n", arr[0], arr[1], arr[2], arr[3], arr[4]);

	char c0 = 1;
	char c1 = 1;
	char c2 = 1;
	unsigned char c3 = 1;
	unsigned char c4 = 1;
	c0 += 127;  // 0b1000000 = -128
	c1 += 125;  // 126
	c2 += 255;  // 0
	c3 += 255;  // 0
	c4 += 256;  // 1
	printf("c0: %d, c1: %d, c2: %d, c3: %d, c4: %d\n", c0, c1, c2, c3, c4);

	return 0;
}

