#include <stdio.h>


class C {
public:
	int mA;

	// ctor + init
	C() : mA(0) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
	}

	~C() { printf("~C()\n"); }

	void mF() { printf("mF()\n"); }
};

C f(void) {
	printf("f()\n");
	return C();
}

int main() {

	C c = f();

	f().mF();
	printf("f().mA 2 == %d\n", f().mA);
	
	return 0;
}
