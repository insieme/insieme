#include <iostream>

class C {
	int mA;
public:
	C() : mA(10) { std::cout << "C()"; }
	~C() { std::cout << "~C()"; mA = 0; }
};

int main() {
	C c;

	C* pC = new C();

	delete pC;
	return 0;
}
