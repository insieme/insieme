#include <stdio.h>
#include <stdint.h>

int main() {

	{
		// try it with signed
		int64_t a = 10;
		int64_t b = 3;

		int64_t c = a / b;

		printf("a=%d\n", a);
		printf("b=%d\n", b);
		printf("a+b=%d\n", a+b);
		printf("a-b=%d\n", a-b);
		printf("a*b=%d\n", a*b);
		printf("a/b=%d\n", a/b);
		printf("a%b=%d\n", a%b);
	}

	{
		// the same for unsigned
		uint64_t a = 10;
		uint64_t b = 3;

		uint64_t c = a / b;

		printf("a=%d\n", a);
		printf("b=%d\n", b);
		printf("a+b=%d\n", a+b);
		printf("a-b=%d\n", a-b);
		printf("a*b=%d\n", a*b);
		printf("a/b=%d\n", a/b);
		printf("a%b=%d\n", a%b);
	}

	{
		// try signed / unsigned combination
		uint64_t a = 10;
		int64_t b = -3;

		uint64_t c = a / b;

		printf("a=%d\n", a);
		printf("b=%d\n", b);
		printf("a+b=%d\n", a+b);
		printf("a-b=%d\n", a-b);
		printf("a*b=%d\n", a*b);
		printf("a/b=%d\n", a/b);
		printf("a%b=%d\n", a%b);
	}

	return 0;
}

