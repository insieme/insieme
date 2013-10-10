#include <stdio.h>
#include <stdlib.h>

struct inner_s;
struct outer_s;

struct inner_s {
	struct outer_s *outer;
	void (*func_ptr)(struct outer_s *_outer);
};


struct outer_s {
	struct inner_s *inner;
};

static void func(struct outer_s *_outer)
{
	printf("Ciao\n");
}

static struct inner_s _inner_static = {0,func};
struct inner_s _inner_global = {0,func};

int main()
{
		void (*func_ptr)(struct outer_s *_outer) = func;

        struct outer_s o;
        struct inner_s i = {0, func};

        o.inner =  0;
        o.inner =  &i;
        //TROUBLEMAKERs
//        o.inner =  &_inner_static;
//       o.inner =  &_inner_global;

		if(o.inner->func_ptr) {
			(*o.inner->func_ptr)(NULL);
		}

        return 0;
}

