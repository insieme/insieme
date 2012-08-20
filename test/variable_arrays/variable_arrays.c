
#include <stdio.h>

struct a {
	int b;
};

int main() {

	unsigned n=10;
	int v[n];

	v[2] = 3;
	
	struct a s[n];
	s[2].b = 10;

}
