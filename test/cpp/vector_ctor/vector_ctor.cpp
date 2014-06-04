#include <stdio.h>

struct C1 { };

struct C2 {
	int x;
	C2() : x(10) {}
};

int main() {
	C1 c1[4];
	C2 c2[4];

	C1 c3[2][3];
	C2 c4[2][3];

	printf("%d\n", sizeof(c1));
	printf("%d\n", sizeof(c2));
	printf("%d %d\n", sizeof(c3), sizeof(c3[0]));
	printf("%d %d\n", sizeof(c4), sizeof(c4[0]));

	return 0;
}
