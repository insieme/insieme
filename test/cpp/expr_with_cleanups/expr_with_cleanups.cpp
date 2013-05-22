#include <stdio.h>


class C {
public:
	int mA;

	// ctor + init
	C(int a=0) : mA(a) {
		printf("C()");
		printf("mA 0 == %d\n", mA);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
	}

	~C() { printf("~C()\n"); }

	void mF() { printf("mF() v: %d \n",mA); }
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
	}

	// envolving functionm
	{
		f().mF();
		printf("f().mA 2 == %d\n", f().mA);
	}

	// return value of inner copy
	{
		C c(2);
		g(c).mF();
	}
	
	return 0;
}
