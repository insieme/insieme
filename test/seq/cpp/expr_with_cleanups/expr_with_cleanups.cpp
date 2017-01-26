#include <stdio.h>


class C {
public:
	int mA;

	// ctor + init
	C(int a=0) : mA(a) {
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA;
	}

	~C() { }

	void mf() { printf("mf() v: %d \n",mA); }
};

C f(void) {
	printf("f()\n");
	return C();
}

C g(C& o){
	C a = o;
	printf("g()\n");
	return a;
}

/////////////////////////////////////////////////////////////////////
//
int main() {

	// tmp obj
	{
		C(4);
	}

	// cpy ctor with temporary
	{
		C c = f();
		c.mf();
	}

	// envolving function
	{
		f().mf();
		printf("f().mA 2 == %d\n", f().mA);
	}

	// return value of inner copy
	{
		C c(2);
		c.mf();
		g(c).mf();
	}
	
	return 0;
}
