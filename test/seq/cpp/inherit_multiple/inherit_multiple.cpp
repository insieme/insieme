#include <stdio.h>

//multiple inheritance
class A {
public:
	A() : aA(0), X(1) {}
	int aA;
	int X;
};

class B {
public:
	B() : aB(0), X(2) {}
	int aB;
	int X;
};

class C : public A, public B {
public:
	C() : aC(0) {}
	int aC;
};

int main() {
	{
		C c;
		printf("c.A::X %d\n", c.A::X);
		printf("c.B::X %d\n", c.B::X);
	}

	return 0;
}
