#include <stdio.h>

class C {
	int mA;
	int mB;
public:
	int mC;

	C(int a) : mA(a), mB(100){
		mC = 1000;
	}

	~C() {
		printf("~C(%d)\n",mA);
		mA = 0; mB = 0; mC = 0;
	}
};

void f() {
	C c(4);
	// call dtor on leaving scope
}

int main() {
	C c1(1);

	{
		C c1(2);
		C c2(3);
		// call dtor of local c1!
		// call dtor on leaving scope
	}

	f();

	return 0;
}
