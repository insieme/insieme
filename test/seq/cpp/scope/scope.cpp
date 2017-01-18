#include <stdio.h>

namespace NS {
	namespace SN{
		typedef int A;
		A a = 11;
	}
}

typedef double A;

NS::SN::A numI = 1;
A numD = 1.0;

// test global var
int globalInt;

class C {
	int cC;
public:
	void usesGlobals(int* x) { cC=0; *x=globalInt; globalInt+=100; }
};

void usesGlobals(int* x) {
	*x = globalInt;
	globalInt++;
}

int main() {
	NS::SN::A numI = 3;
	A numD = 3.0;

	printf("numI 3 == %d\n", numI);
	printf("::numI 1 == %d\n", ::numI);
	printf("numD 3.0 == %f\n", numD);
	printf("::numD 1.0 == %f\n", ::numD);
	printf("::globalInt 0 == %d\n", globalInt);

	{
		int globalInt = 0;
		//test namespace scoping
		NS::SN::A numI = 5;
		//test general scoping
		A numD = 5.0;

		printf("numI 5 == %d\n", numI);
		printf("::numI 1 == %d\n", ::numI);

		printf("numD 5.0 == %f\n", numD);
		printf("::numD 1.0 == %f\n", ::numD);

		// test global var
		globalInt = 100;
		printf("globalInt 100 == %d\n", globalInt);
		::globalInt = globalInt;
		printf("::globalInt 100 == %d\n", ::globalInt);
	}

	// test global var
	int x = globalInt;	// x==100
	printf("x 100 == %d\n", x);

	usesGlobals(&x);	// x == 100, globalInt == 101
	printf("x 100 == %d\n", x);
	printf("::globalInt 101 == %d\n", globalInt);

	C c;
	c.usesGlobals(&x);	// x == 101, gloalInt == 201
	printf("x 101 == %d\n", x);
	printf("::globalInt 201 == %d\n", globalInt);

	printf("NS::SN::a 11 == %d\n", NS::SN::a);

	return 0;
}
