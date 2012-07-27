#include <stdio.h>

int globalVar;

class C {
	int mA;
public:
	C() : mA(10) { printf("C()"); }
	~C() { printf("~C()"); mA = 0; }
};

class GC {
	int mA;
public:
	GC() : mA(10) { globalVar = 0; printf("GC()"); }
	~GC() { printf("~GC()"); mA = 0; }
};

int main() {

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

	//test mem-alloc with globalVar used in ctor
	{
		GC* pC = new GC();
		delete pC;
	}
	{
		GC* pC = new GC[5];
		delete[] pC;
	}

	return 0;
}
