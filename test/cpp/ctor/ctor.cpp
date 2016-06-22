#include <stdio.h>
#include <stdlib.h>

class C {
	int mA;
	int mB;
	void * mC;
	int * mD;
public:

	// ctor + init
	C() : mA(0), mB(1){
		printf("C()");
		printf("mA 0 == %d mB 1 == %d\n", mA, mB);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA; mB = c.mB; mC = c.mC;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
		printf("mB==c.mB -- %d == %d\n", mB, c.mB);
	}

	// ctor + default args
	// doing some "fancy" stuff in the initializer
	C(int a, int b= 5) : mA(a), mB(mA+b) {
		printf("C(int a, int b) -- default arg\n");
		printf("mA %d == %d, mB %d  == %d\n", this->mA, a, mB, mA+b);
	}

	//ctor + void pointer init
	// doing some "fancy" stuff in the initializer
	C(void * __arg) : mC(__arg) , mD(&mA){
		printf("C(void * __arg)\n");
		printf("mC != NULL -> %d\n", mC!=NULL);
		printf("mD != NULL -> %d\n", mD!=NULL);
		printf("mD == &mA -> %d\n", mD==&mA);
	}

	//ctor + pointer init
	C(int * __arg) : mD(__arg) {
		printf("C(int * __arg)");
		printf("mD 0 == %d\n", *mD);
	}
};

class D {

public:
	int ma;
	D() : ma(12) {
		printf("D()\n");
	}
	D(const D& o) { printf("D(const D&)\n"); }
	D(int a) : ma(a) {
		printf("D(int)\n");
		printf("ma == %d\n", ma);
	}
};

class E {
public:
	int x;
	D dD;		//doing some strange stuff with initializers
	D& dr;		//doing some strange stuff with initializers
	const D& cdr;
	E() : dr(dD), cdr(dr) {
		printf("E()\n");
		printf("dD.ma == %d\n", dD.ma);
		printf("dr.ma == dD.ma -> %d\n", dr.ma == dD.ma);
		printf("cdr.ma == dD.ma -> %d\n", cdr.ma == dD.ma);
		dr = dD;
	}
	E(const E& o): cdr(dD), dr(dD) { printf("E(const E&)\n"); }
	E(int e) : dD(e), cdr(dD), dr(dD) {
		printf("E(int)\n");
		printf("dD.ma == %d\n", dD.ma);
	}

	E(int e, int d) : x(d), dD(x), cdr(dD), dr(dD) {
		printf("E(int, int)\n");
		printf("dD.ma == %d\n", dD.ma);
	}
};

struct X {
	const C& c;
	X(const C& c) : c(c) {}
};

//tests private struct used 
//as ctor default argument
class Y {
    struct Z {};
public:
    Y(Z z=Z()) { printf("generated object Y\n"); }
};

int main() {
	// ctor + init
	{
		printf("Ctor+init\n");
		C ci1;
		X x(ci1);
	}
	{
		printf("\nArray of objects\n");
		C cA[5];
	}
	
	//ctor + pointer init
	{
		printf("ctor void pointer init\n");
		void * x = malloc(sizeof(char));
		C cp1(x);
		free(x);
		int * y = (int *) malloc(sizeof(int));
		*y=10;
		C cp2(y);
		free(y);
	}

	{
		// copy ctor
		C ci1;
		printf("\nCopy Ctor\n");
		C cc1(ci1);
	}

	{
		// ctor + default arg
		printf("\nCtor+default args\n");
		C cd1(1);
		C cd2(1,2);
	}
	{
		// D has comparable ctors as C but is used as member in E
		printf("\nd:\n\n");
		D d1;
		D d2(10);
	
		printf("\nd da[3]:\n\n");
		D da[2];
	}
	{
		// Test class member init with ctor
		// E.dD needs ctor of class D
		printf("\nE():\n\n");
		E e1;
		printf("\nE(int):\n\n");
		E e2(10);
		printf("\nE eA[3]:\n\n");
		E eA[3];
	}

	{
		Y y;
	}
		
	E e2(10, 10);
	
	return 0;
}
