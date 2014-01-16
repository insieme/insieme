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
/*
	// copy ctor
	C(const C& c) {
		mA = c.mA; mB = c.mB; mC = c.mC;
		printf("C(cont C& c)");
		printf("mA==c.mA -- %d == %d\n", mA, c.mA);
		printf("mB==c.mB -- %d == %d\n", mB, c.mB);
	}*/

	// ctor + default args
	C(int a, int b= 5) : mA(a), mB(b) {
		printf("C(int a)");
		printf("mA 0 == %d, mB 5 == %d\n", mA, mB);
	}

	//ctor + void pointer init
	C(void * __arg) : mC(__arg) {
		printf("C(void * __arg)");
		printf("mC != NULL -> %d\n", mC!=NULL);
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
	D dD;
	E() {
		printf("E()\n");
		printf("dD.ma == %d\n", dD.ma);
	}
	E(const E& o) { printf("E(const E&)\n"); }
	E(int e) : dD(e) {
		printf("E(int)\n");
		printf("dD.ma == %d\n", dD.ma);
	}
};

int main() {
	// ctor + init
	{
		printf("Ctor+init\n");
		C ci1;
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
	//	printf("\nD:\n\n");
	//	D d1;
	//	D d2(10);
	//
	//	printf("\nD dA[3]:\n\n");
	//	D dA[2];
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
	
	return 0;
}
