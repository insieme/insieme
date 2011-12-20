#include <iostream>

class C {
	int mA;
public:
	C() : mA(10) { std::cout << "C()"; }
	~C() { std::cout << "~C()"; mA = 0; }
};

int main() {

	{
		int* pi = new int;
		delete pi;

		C* pC = new C();
		delete pC;
	}

	{
		//not supported at the moment
//		int* pi = new int[5];
//		delete[] pi;

		//not supported at the moment
//		C* pC1 = new C[5];
//		delete[] pC;
	}
	return 0;
}
