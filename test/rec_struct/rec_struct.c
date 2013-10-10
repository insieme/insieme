#include <stdio.h>

struct inner_s;
struct outer_s;

struct inner_s {
		struct outer_s* _outer;
		int i;
};

struct outer_s {
        struct inner_s *_inner;
        int o;
};

int main()
{
		struct inner_s i = (struct inner_s){0, 55};
        struct outer_s o = (struct outer_s){0, 11};

		o._inner =  &i;
        i._outer =  &o;

		printf("%d\n", o.o);
		printf("%d\n", o._inner->i);

		printf("%d\n", i.i);
		printf("%d\n", i._outer->o);


        return 0;
}

