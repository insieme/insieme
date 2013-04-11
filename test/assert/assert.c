#include <assert.h>
#include <stdio.h>


struct obj{
	int a;
	float b;
};




int main(int argc, char ** argv){

	struct obj o;
	int a = 34;

	o.a = 1;
	o.b = 3.0;

	assert (a == 34    && "int a is not 34");
	printf("assert 1 was ok\n");
	assert (o.a == 1   && "o.a is not 1");
	printf("assert 2 was ok\n");
	assert (o.b == 3.0 && "o.b is not 3.0");
	printf("assert 3 was ok\n");

	return 0;
}
