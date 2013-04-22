#include <stdio.h>

//int globalVar;

class C {
public:
	int mA;

	// ctor + init
	C() : mA(0) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
//		globalVar=1;
		//printf("globalVar 1 == %d\n", globalVar);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
	}

	~C() { printf("~C()"); }

	void mF() { printf("mF()"); }
};

C f(void) {
	printf("f()\n");
	return C();
}

int main() {

	C c = f();
/*	
	f().mF();
	printf("f().mC 2 == %d\n", f().mC);
*/	
	return 0;
}
