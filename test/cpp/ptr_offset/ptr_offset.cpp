#include <stdio.h>

int main() {

	//int a[] = { 1, 2, 3, 4, 5, 6, 7 };
	char a[] = { 'a', 'b', 'c', 'd', 'e', 'f', 'g' };

	char* p = a;
	char**p2 = &p;

	p2 = p2 + 2;

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

	return 0;
}
