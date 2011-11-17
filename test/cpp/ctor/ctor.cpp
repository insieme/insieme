#include <iostream>

class C {
	int mA;
	int mB;
public:
	int mC;

	// ctor + init
	C() : mA(0), mB(1), mC(2) {
		std::cout << "C()";
		std::cout << "mA 0 == " << mA;
		std::cout << "mB 1 == " << mB;
		std::cout << "mC 2 == " << mC;
		mC += mA + mB;
		std::cout << "mC+=mA+mB 3 == " << mC;
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA; mB = c.mB; mC = c.mC;
		std::cout << "C(cont C& c)";
		std::cout << "mA==c.mA " << mA << " == " << c.mA;
		std::cout << "mB==c.mB " << mB << " == " << c.mB;
		std::cout << "mC==c.mC " << mC << " == " << c.mC;
	}

	// ctor + default args
	C(int a, int b=10, int c=100) : mA(0), mB(1), mC(2) {
		std::cout << "C(int a, int b=10, int c=100)";
		std::cout << "mA 0 == " << mA;
		std::cout << "mB 1 == " << mB;
		std::cout << "mC 2 == " << mC;
		mC = a + b + c;
		std::cout << "mC = a + b = "<< a << "+"<< b <<" = " << mC;
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
