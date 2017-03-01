#include<stdio.h>
#include<stdint.h>

int f(long x) {
	return 0;
}

int g(long long x) {
	return 1;
}

int h(int64_t x) {
	return 2;
}

int main() {

	printf("int:       %lu\n", sizeof(int));
	printf("long:      %lu\n", sizeof(long));
	printf("long long: %lu\n", sizeof(long long));


	int a = 4;
	long b = 6;
	long long c = 7;

	f(a);
	f(b);
	f(c);

	g(a);
	g(b);
	g(c);

	h(a);
	h(b);
	h(c);

	return 0;
}
