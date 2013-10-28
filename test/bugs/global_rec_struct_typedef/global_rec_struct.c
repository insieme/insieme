#include <stdio.h>
#include <stdlib.h>

struct inner_s;
struct outer_s;

typedef struct inner_s {
	struct outer_s *outer;
	int i;
} inner_t;


typedef struct outer_s {
	struct inner_s *inner;
	int o;
} outer_t;

static inner_t _inner_static = {0,1};
static outer_t _outer_static = {0,2};
inner_t _inner_global = {0,3};
outer_t _outer_global = {0,4};

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

