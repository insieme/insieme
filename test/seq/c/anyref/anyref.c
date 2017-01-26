
#include <stdio.h>
#include <assert.h>

int main(int argc, char* argv[]) {

	int* a=NULL,*b=a;

	assert(a == b && "This is a semantic ERROR!");

	void* ptr;
	ptr = a;

	assert(!ptr && "This is the second semantic ERROR!");

	int val = 10;
	ptr = &val;
	a = ptr;
	b = &val;

	assert(*a == *b && "This is the second semantic ERROR!");

	return 0;
}
