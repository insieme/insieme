#include <stddef.h>
#include <stdio.h>

int f(int* a) {
	return *a++;
}

ptrdiff_t ptr_diff(int* p, int *q) {
	return p - q;
}

int main(int argc, char* argv[]) {


	{
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
	{
		int a[5] = {1};
		int *p;
		int *q;

		p = &a[3];
		q = &a[1];
		int  diff;
		diff = p-q;
		printf(" %ld == %d\n", p-q, diff);
		diff = q-p;
		printf(" %ld == %d\n", q-p, diff);

		ptrdiff_t ptrDiff;
		ptrDiff = ptr_diff(p,q);
		printf(" %ld == %ld\n", p-q, ptrDiff);
		ptrDiff = ptr_diff(q,p);
		printf(" %ld == %ld\n", q-p, ptrDiff);

	}
	
}
