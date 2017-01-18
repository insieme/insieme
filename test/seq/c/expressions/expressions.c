#include <stdio.h>

#define TEST(_a, _op, _b) printf("%3d\t%s %d\n", __LINE__, #_op, a _op b)

int main() {


	int a = 0;
	int b = 1;
	float f  = 0.0;
	double g = 1.0;


	// test the same type
	TEST(a,+,b);
	TEST(a,-,b);
	TEST(a,*,b);
	TEST(a,/,b);

	TEST(a,<,b);
	TEST(a,<=,b);
	TEST(a,>,b);
	TEST(a,>=,b);


	// test different types
	TEST(a,<,f);
	TEST(a,<=,f);
	TEST(a,>,f);
	TEST(a,>=,f);

	TEST(f,<,a);
	TEST(f,<=,a);
	TEST(f,>,a);
	TEST(f,>=,a);

	//printf("%d\n", ~(a<=f));

	return 0;
}
