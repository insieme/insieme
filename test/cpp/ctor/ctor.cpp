#include <stdio.h>

class C {
	int mA;
	int mB;
public:
	int mC;

	// ctor + init
	C() : mA(0), mB(1), mC(2) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
		printf("mB 1 == %d\n", mB);
		printf("mC 2 == %d\n", mC);
		mC += mA + mB;
		printf("mC+=mA+mB 3 == %d\n", mC);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA; mB = c.mB; mC = c.mC;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
		printf("mB==c.mB -- %d == %d\n", mB, c.mB);
		printf("mC==c.mC -- %d == %d\n", mC, c.mC);
	}

	// ctor + default args
	C(int a, int b=10, int c=100) : mA(0), mB(1), mC(2) {
		printf("C(int a, int b=10, int c=100)");
		printf("mA 0 == %d\n", mA);
		printf("mB 1 == %d\n", mB);
		printf("mC 2 == %d\n", mC);
		mC = a + b + c;
		printf("mC = a + b + c = %d + %d + %d = %d\n", a, b, c, mC);
	}
};

int main() {

	// ctor + init
	C ci1;

	// copy ctor
	C cc1(ci1);

	// ctor + default arg
	C cd1(1);
	C cd2(1,2);
	C cd3(1,2,3);

	return 0;
}
