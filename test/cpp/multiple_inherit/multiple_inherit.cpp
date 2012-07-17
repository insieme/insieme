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

class Base1 {
public:
	void func1() { printf("Base1::func1"); }
	~Base1() {};
};

class Base2 {
public:
	void func2() { printf("Base2::func2"); }
	~Base2() {};
};

class Class : public Base1, public Base2 {
public:
	void func1() { printf("Class::func1"); }
	void func2() { printf("Class::func2"); }
	~Class() { printf("Class::~Class()\n"); }
};


int main() {
	{
		C c;
		printf("c.A::X %d\n", c.A::X);
		printf("c.B::X %d\n", c.B::X);
	}
	{
		Class c;
		c.func1();
		c.func2();

		Base1 b1;
		b1.func1();
		Base2 b2;
		b2.func2();

		Base1 *pb1 = &c;
		pb1->func1();

		Base2& rb2 = c;
		rb2.func2();
	}

	return 0;
}
