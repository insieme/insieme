#include <stdio.h>

int globalVar;

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
		globalVar=1;
		printf("globalVar 1 == %d\n", globalVar);
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

class D {

public:
	int ma;
	D() {
		printf("D()\n");
		globalVar = 0;
	}
	D(const D& o) { printf("D(const D&)\n"); }
	D(int a) : ma(a) {
		globalVar = 0;
		printf("D(int)\n");
		printf("ma == %d\n", ma);
	}
};

class E {
public:
	D dD;
	E() {
		printf("E()\n");
		printf("dD.ma == %d\n", dD.ma);
	}
	E(const E& o) { printf("E(const E&)\n"); }
	E(int e) : dD(e) {
		printf("E(int)\n");
		printf("dD.ma == %d\n", dD.ma);
	}
};

int main() {

	// ctor + init
	printf("Ctor+init\n");
	C ci1;

	printf("\nArray of objects\n");
	C cA[5];

	// copy ctor
	printf("\nCopy Ctor\n");
	C cc1(ci1);

	// ctor + default arg
	printf("\nCtor+default args\n");
	C cd1(1);
	C cd2(1,2);
	C cd3(1,2,3);

	// D has comparable ctors as C but is used as member in E
//	printf("\nD:\n\n");
//	D d1;
//	D d2(10);
//
//	printf("\nD dA[3]:\n\n");
//	D dA[2];

	// Test class member init with ctor
	// E.dD needs ctor of class D
	printf("\nE():\n\n");
	E e1;

	printf("\nE(int):\n\n");
	E e2(10);

	printf("\nE eA[3]:\n\n");
	E eA[3];

	return 0;
}
