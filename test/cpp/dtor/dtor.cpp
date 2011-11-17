#include <iostream>

class C {
	int mA;
	int mB;
public:
	int mC;

	C() : mA(10), mB(100), mC(100) {}

	~C() {
		std::cout << "~C()";
		mA = 0; mB = 0; mC = 0;
	}
};

void f() {
	C c;
	// call dtor on leaving scope
}

int main() {
	C c1;

	{
		C c1;
		C c2;
		// call dtor of local c1!
		// call dtor on leaving scope
	}

	return 0;
}
