#include <stdio.h>

/////////////////////////////////////////////////
//multiple inheritance
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

/////////////////////////////////////////////////
class J: public C{
};

int main() {
	{
		J j;
		printf("j.C::A::X %d\n", j.C::A::X);
	}

	return 0;
}
