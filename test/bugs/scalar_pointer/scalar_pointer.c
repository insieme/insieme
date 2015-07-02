#include <stdlib.h>
#include <stdio.h>

//  WARNING: this is a memory layout dependent code, it might not work on different architecture
//  than default

int main (){
	int a[2] ={0,1};
	size_t mem;
	int* ptr;
	mem = a;

	// this will never be the same since is a realocable memory location
	//printf("%p\n", mem);

	ptr = mem;

	if (ptr == a)
		printf ("pointers equal\n");
	if (*ptr == a[0])
		printf ("values equal\n");

	ptr = mem+sizeof(int);

	if (*ptr == a[1])
		printf ("values equal\n");

	// test integral to pointer casts with negative value
	int* nptr;
	nptr = (int*) -1;
        if(nptr == -1) {
		printf("nptr == -1\n");
	} else {
		printf("nptr != -1\n");
	}

	return 0;
}
