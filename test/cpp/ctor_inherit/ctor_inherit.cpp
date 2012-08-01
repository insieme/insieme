#include <stdio.h>

int globalVar;

class A {
public:
	A() : aA(0), X(1) { printf("A()\n"); }
	int aA;
	int X;
};

class B {
public:
	B() : aB(0), X(2) { printf("B()\n");}
	int aB;
	int X;
};

//multiple inheritance
class MC : public A, public B {
public:
	MC() : aMC(0) { printf("MC()\n");}
	int aMC;
};

//single inheritance
class C : public A {
public:
	C() : aC(0) { printf("C()\n");}
	int aC;
};

class GA {
public:
	GA() : aA(0), X(1) { globalVar = 1; printf("A()\n"); }
	int aA;
	int X;
};

class GB {
public:
	GB() : aB(0), X(2) { globalVar = 2; printf("B()\n");}
	int aB;
	int X;
};

//multiple inheritance + globalVar
class GMC : public GA, public GB {
public:
	GMC() : aMC(0) { printf("MC()\n");}
	int aMC;
};

//single inheritance + globalVar
class GC : public GA {
public:
	GC() : aC(0) { printf("C()\n");}
	int aC;
};

int main() {

	// test calling of base-class ctors
	{
		MC mc;
		printf("mc.A::X %d\n", mc.A::X);
		printf("mc.B::X %d\n", mc.B::X);
	}
	{
		C c;
		printf("c.A::X %d\n", c.A::X);
	}

	// GMC/GC test handling of globalVar
	// for ctor calling and single/multiple inheritance
	{
		GMC mc;
		printf("mc.GA::X %d\n", mc.GA::X);
		printf("mc.GB::X %d\n", mc.GB::X);
		printf("globalVar %d\n", globalVar);
	}
	{
		GC c;
		printf("c.GA::X %d\n", c.GA::X);
		printf("globalVar %d\n", globalVar);
	}
}
