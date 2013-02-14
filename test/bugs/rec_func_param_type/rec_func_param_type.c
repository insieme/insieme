#include <stdio.h>
#include <stdlib.h>

struct outer_s;

typedef struct inner_s {
        void (*func_ptr)(struct outer_s *_outer);
} inner_t;

typedef struct outer_s {
        inner_t *inner;
} outer_t;

static void func(outer_t *_outer)
{
        printf("Ciao\n");
}

static inner_t _inner = {func};

int main()
{
        outer_t _outer;

        _outer.inner =  &_inner;

        (*_outer.inner->func_ptr)(NULL);

        return 0;
}

