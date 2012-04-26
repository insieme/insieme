#include <stdio.h>

typedef union {
	char e[2];
	struct { char first; char second; };
	struct { short single; };
} pair;

struct weird {
	union { int i; char c4[4]; };
	union { short s; char c2[2]; };
};

int main (int argc, char* argv[]) { 

	pair p = {1, 2};
	p.first = 10;

	printf("%c\n", p.first);
	printf("%d\n", p.single);
	
	struct weird w;
	w.i = 10;
	w.s = 20;

	printf("%d, %d, %d, %d\n", w.c4[0], w.c4[1], w.c4[2], w.c4[3]);
	printf("%d, %d\n", w.c2[0], w.c2[1]);

}
