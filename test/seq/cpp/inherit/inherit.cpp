#include <stdio.h>

/////////////////////////////////////////////////
class A {
public:
	A() : aA(0), X(1) {}
	int aA;
	int X;
};


class C : public A {
public:
	C() : aC(0) {}
	int aC;
};

int main() {
	{
		C c;
		printf("c.A::X %d\n", c.A::X);
	}

	return 0;
}
