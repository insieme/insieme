#include <stdio.h>
#include <stdlib.h>

struct inner_s;
struct outer_s;

struct inner_s {
	struct outer_s *outer;
	int i;
};


struct outer_s {
	struct inner_s *inner;
	int o;
};

static struct inner_s _inner_static = {0,1};
static struct outer_s _outer_static = {0,2};
struct inner_s _inner_global = {0,3};
struct outer_s _outer_global = {0,4};

int main()
{
	{

		_inner_static.outer = &_outer_static;
		_outer_static.inner = &_inner_static;

	    printf("%d\n", _outer_static.o);
		printf("%d\n", _outer_static.inner->i);

		printf("%d\n", _inner_static.i);
		printf("%d\n", _inner_static.outer->o);
	}
	{
		_inner_global.outer = &_outer_global;
		_outer_global.inner = &_inner_global;

	    printf("%d\n", _outer_global.o);
		printf("%d\n", _outer_global.inner->i);

		printf("%d\n", _inner_global.i);
		printf("%d\n", _inner_global.outer->o);
	}

	return 0;
}

