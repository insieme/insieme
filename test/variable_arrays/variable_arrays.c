
#include <stdio.h>

struct a {
	int b;
};

int main() {

	unsigned n=10;
	int v[n];

	v[2] = 3;
	
	struct a s[n + 123];
	s[2].b = 10;

	
	
	int* x = malloc(1200);
	x[2] = 12;
	free(x);


	int y[230];
	y[123] = 23;


}
