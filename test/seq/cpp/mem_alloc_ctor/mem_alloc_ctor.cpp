#include <stdio.h>

class C {
	int mA;
	int mB;
public:

	// ctor + init
	C() : mA(0), mB(1) {
		//printf("C()");
		//printf("mA 0 == %d\n", mA);
		//printf("mB 1 == %d\n", mB);
	}

	// copy ctor
	C(const C& c) {
		mA = c.mA; mB = c.mB;
		//printf("C(cont C& c)");
		//printf("mA==c.mA -- %d == %d\n", mA, c.mA);
		//printf("mB==c.mB -- %d == %d\n", mB, c.mB);
	}

	// ctor + default args
	C(int a, int b=10) : mA(0), mB(1) {
		//printf("C(int a, int b=10, int c=100)");
		//printf("mA 0 == %d\n", mA);
		//printf("mB 1 == %d\n", mB);
	}
	~C(){
	}
};

class D {

public:
	int ma;
	D() {
		//printf("D()\n");
	}
	D(const D& o) { 
		//printf("D(const D&)\n"); 
	}
	D(int a) : ma(a) {
		//printf("D(int)\n");
		//printf("ma == %d\n", ma);
	}
};

class E {
public:
	D dD;
	E() {
		//printf("E()\n");
		//printf("dD.ma == %d\n", dD.ma);
	}
	E(const E& o) { 
		//printf("E(const E&)\n"); 
	}
	E(int e) : dD(e) {
		//printf("E(int)\n");
		//printf("dD.ma == %d\n", dD.ma);
	}
};

int main() {

	{
		// ctor + init
		//printf("Ctor+init\n");
		C *ci1 = new C();
		delete(ci1);
	}

	{
		//printf("\nArray of objects\n");
		C *cA = new C[5];
		delete[] cA;
	}

	{
		// ctor + default arg
		//printf("\nCtor+default args\n");
		C *cd1 = new C(1);
		C *cd2 = new C(1,2);
		delete cd1;
		delete cd2;
	}

	{
		//printf("\nD:\n\n");
		D *d1 = new D();
		D *d2 = new D(10);
		delete d1;
		delete d2;
	}

	{
		//printf("\nD dA[3]:\n\n");
		D *dA = new D[2];
		delete[] dA;
	}

	{
		//printf("\nE():\n\n");
		E *e1 = new E();
		//printf("\nE(int):\n\n");
		E *e2 = new E(10);

		delete e1;
		delete e2;
	}

	{
		//printf("\nE eA[3]:\n\n");
		E *eA = new E[3];
		delete[] eA;
	}
	return 0;
}
