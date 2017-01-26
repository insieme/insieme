
#include <stdio.h>


void* data;

int main() {


	int a = 4;

	{
		// test the global
		data = &a;

		*(int*)data = 2;
		printf("a=%d\n", a);
	}

	{
		// test a local variable
		void* x = &a;

		*(int*)data = 8;
		printf("a=%d\n", a);
	}

	return 0;
}
