#include <stdio.h>

class C {
	int mA;

public:
	int mC;

	C(int a) : mA(a){}

	~C() {
		printf("~C(%d)",mA);
		mA = 0;	
	}
};

void f() {
	C(2);
	// call the destuctor of the temporary object
	C c(3);
	//call destructor of c
}

int main() {

	C c(1);
	f();
	C(4);
	//call the destructor of the temporary object
	//call destructor of c on leaving scope
	return 0;
}
