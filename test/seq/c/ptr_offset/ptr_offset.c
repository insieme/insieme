#include <stdio.h>

int main() {

	int a[] = { 1, 2, 3, 4, 5, 6, 7 };

	int* p = a;

	// signed offsets
	printf("%d", *p);
	p++;
	printf("%d", *p);
	++p;
	printf("%d", *p);
	p += 2;
	printf("%d", *p);
	p = p + 2;
	printf("%d", *p);

	p = p - 2;
	printf("%d", *p);
	p -= 2;
	printf("%d", *p);
	--p;
	printf("%d", *p);
	p--;
	printf("%d", *p);

	printf("\n");

	// unsigned offsets
	p += 2u;
	printf("%d", *p);
	p = p + 2u;
	printf("%d", *p);

	p = p - 2u;
	printf("%d", *p);
	p -= 2u;
	printf("%d", *p);

	printf("\n");


	// long long offsets
	p += 2ll;
	printf("%d", *p);
	p = p + 2ll;
	printf("%d", *p);

	p = p - 2ll;
	printf("%d", *p);
	p -= 2ll;
	printf("%d", *p);

	printf("\n");


	// unsigned long long offsets
	p += 2ull;
	printf("%d", *p);
	p = p + 2ull;
	printf("%d", *p);

	p = p - 2ull;
	printf("%d", *p);
	p -= 2ull;
	printf("%d", *p);

	printf("\n");

	// test integer cast
	unsigned long addr = (unsigned long)p;
	int* p2 = (int*)addr;
	printf("%d",*p2);

	return 0;
}
