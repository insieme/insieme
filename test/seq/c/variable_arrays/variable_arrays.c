#include <stdlib.h>
#include <stdio.h>

struct a {
	int b;
};

int main() {

	unsigned n=10;
	int v[n];

	v[2] = 3;
	
	struct a s[n + 123];
	s[2].b = v[2];
	
	int* x = malloc(1200);
	x[2] = s[2].b;
	printf("%d\n", x[2]);

	free(x);

	int y[230];
	y[123] = 23;


}
