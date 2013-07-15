#include <stdio.h>

struct test {
      void *(*bzalloc)();
} t;

int main(int argc, char** argv)
{
	t.bzalloc = NULL;

	if(t.bzalloc == NULL) 
		return 0;
	else
		return 1;
}
