#include <iostream>

namespace NS {
	namespace SN{
		typedef int A;
	}
}

typedef double A;

NS::SN::A numI = 1;
A numD = 1.0;

// test global var
int globalInt;

class C {
	void usesGlobals(int* x) { *x=globalInt; globalInt+=100; }
};

void usesGlobals(int* x) {
	*x = globalInt;
	globalInt++;
}

int main() {
	NS::SN::A numI = 3;
	A numD = 3.0;

	std::cout << "numI 3 == " << numI;
	std::cout << "numD 3.0 == " << numD;
	std::cout << "globalInt 0 == " << globalInt;

	{
		//test namespace scoping
		NS::SN::A numI = 5;
		//test general scoping
		A numD = 5.0;

		std::cout << "numI 5 == " << numI;
		std::cout << "numD 5.0 == " << numD;

		// test global var
		globalInt = 100;
		std::cout << "globalInt 100 == " << globalInt;
	}

	// test global var
	int x = globalInt;	// x==100
	std::cout << "x 100 == " << x;

	usesGlobals(&x);
	std::cout << "globalInt 101 == " << globalInt;

	C c;
	c.usesGlobals(&x);
	std::cout << "globalInt 201 == " << globalInt;

	return 0;
}
