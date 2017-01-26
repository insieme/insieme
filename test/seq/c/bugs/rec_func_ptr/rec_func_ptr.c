#include <stdio.h>

typedef struct rec_s {
	//BUGGY:
	struct rec_s* (*funcp)(struct rec_s* r);
	
	//WORKAROUND: cast to/from void*
	//void* (*funcp)(void* r);
} rec;

int main()
{
	rec t1;
	rec test = {0};

	return 0;
}
