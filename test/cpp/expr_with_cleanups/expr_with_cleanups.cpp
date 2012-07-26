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

	~C() { printf("~C()"); }
};

C f(void) {
	printf("f()\n");
	return C();
}

int main() {

	C c = f();
	
	f().mC();
	printf("f().mC 2 == %d\n", f().mC);
	
	return 0;
}
