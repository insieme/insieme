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
        struct outer_s o = {0, 11};
		struct inner_s i;

		o._inner =  &i;
        i._outer =  &o;

		printf("%d\n", o.o);
		printf("%d\n", o._inner->i);

		printf("%d\n", i.i);
		printf("%d\n", i._outer->o);


        return 0;
}

