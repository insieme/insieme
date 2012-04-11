#include <stdio.h>

typedef union {
	char e[2];
	struct { char first; char second; };
	struct { short single; };
} pair;

int main (int argc, char* argv[]) { 

	pair p;
	p.first = 10;

	printf("%c\n", p.first);
	printf("%d\n", p.single);
	
}
