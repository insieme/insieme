#include <stdio.h>

struct inner_s;
struct outer_s;

typedef struct inner_s {
		struct outer_s* _outer;
} inner_t;

typedef struct outer_s {
        struct inner_s *_inner;
} outer_t;

int main()
{
	{
		inner_t i = {0};
        outer_t o = {0};
		o._inner =  &i;
        i._outer =  &o;
	}
	{
		inner_t i = (struct inner_s){0};
        outer_t o = (struct outer_s){0};
		o._inner =  &i;
        i._outer =  &o;
	}
	{
		struct inner_s i = {0};
        struct outer_s o = {0};
		o._inner =  &i;
        i._outer =  &o;
	}
	{
		struct inner_s i = (struct inner_s){0};
        struct outer_s o = (struct outer_s){0};
		o._inner =  &i;
        i._outer =  &o;
	}
        return 0;
}

