#include <stdio.h>

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

int main() {
	{
		MC mc;
		printf("mc.A::X %d\n", mc.A::X);
		printf("mc.B::X %d\n", mc.B::X);
	}
	{
		C c;
		printf("c.A::X %d\n", c.A::X);
	}
}
