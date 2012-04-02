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


int main() {

	C(1);
	//temporary destroyed here
	C(2);
	//temporary destroyed here
	C obj2(3);
	C(4);
	//temporary destroyed here
	C obj4(5);
	C(6);
	//temporary destroyed here
	C obj6(7);
	//obj2 obj4 obj6 destroyed in reverse order when leaving scope
	return 0;
}
