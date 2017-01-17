#include <stdio.h>
#include <stdlib.h>

struct test {
	void *v;
	int * i;
};

int main(int argc, char** argv)
{
	struct test t;
	void *p;

	t.i = 0;
	t.v = 0;
	p = 0;
		
	if(t.i == t.v && t.v == p)
		return 0;

	return 1;
}
