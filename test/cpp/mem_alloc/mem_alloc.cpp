#include <stdio.h>


class C {
	int mA;
public:
	C() : mA(10) { printf("C()"); }
	~C() { printf("~C()"); mA = 0; }
};

int main() {
	{
		int i= 7;
		int* pi = new int(i);
		delete pi;
	}

	{
		int* pi = new int;
		delete pi;

		C* pC = new C();
		delete pC;
	}

	{
		int* pi = new int[5];
		delete[] pi;

		C* pC = new C[5];
		delete[] pC;
	}

	return 0;
}
