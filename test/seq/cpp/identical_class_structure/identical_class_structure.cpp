
#include <iostream>


struct A {
	A() { std::cout << "A()"; }
};

struct B {
	B() { std::cout << "B()"; }
};

struct C {
	int a;
	C() { std::cout << "C()"; }
};

struct D {
	int a;
	D() { std::cout << "D()"; }
};


int main() {

	A a;
	B b;
	C c;
	D d;

	return 0;
}


