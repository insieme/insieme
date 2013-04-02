#include <stdio.h>

int globalVar;

class C {
	int mA;
	int mB;
	int mC;
public:

	// ctor + init
	C() : mA(0), mB(1), mC(2) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
		printf("mB 1 == %d\n", mB);
		printf("mC 2 == %d\n", mC);
		mC += mA + mB;
		printf("mC+=mA+mB 3 == %d\n", mC);
		globalVar=1;
		printf("globalVar 1 == %d\n", globalVar);
	}

	C(int i) : mA(0), mB(1), mC(2) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
		printf("mB 1 == %d\n", mB);
		printf("mC 2 == %d\n", mC);
		mC += mA + mB;
		printf("mC+=mA+mB 3 == %d\n", mC);
		globalVar=i;
		printf("globalVar 1 == %d\n", globalVar);
	}
	void memberFunc(int x) { globalVar = x;}

	//FIXME: globalaccess in DTOR
	//~C() { globalVar = 0; }
};

int main() {

	// ctor + init
	printf("Ctor+init\n");
	C ci1;

	ci1.memberFunc(1);
	
	C ci2(1);

	return 0;
}
