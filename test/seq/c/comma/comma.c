#include <stdio.h>

int f(int x) {
	int y;
	return (y = x++);
}

int g(int x) {
	printf("%d\n", x);
	f(x);
	printf("%d\n", x);
	return x;
}

int main() {
	
	int x=0;
	int y=0;

	{
		printf("%d\n", x =y);
	}		
	{
		printf("%d\n", (x, y++, y));
	}		
	{
		int z;
		z = g ((x=1+3, y += x, y));
		printf("%d\n", z);
	}

	return 0;
}
