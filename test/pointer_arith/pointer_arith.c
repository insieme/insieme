
#include <stdio.h>

int f(int* a) {
	return *a++;
}

int main(int argc, char* argv[]) {


	int a[] = { 0, 3, 6, 9 };

	int* a_ptr;
	a_ptr = a;
	a_ptr += 2;
	printf("%d == %d\n", a[2], *a_ptr);

	a_ptr -= 2;
	printf("%d == %d\n", a[0], *a_ptr);

	printf("%d == %d\n", a[0], *(a_ptr++));

	printf("%d == %d\n", a[2], *(++a_ptr));

	*(a_ptr) = 4;
	printf("%d == %d\n", a[2], 4);

	a_ptr = a_ptr + 1;
	printf("%d == %d\n", a[3], *a_ptr);

	printf("%d == %d\n", a[3], f(a_ptr));
	
}
