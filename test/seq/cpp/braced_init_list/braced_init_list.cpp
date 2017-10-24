
#include <iostream>


// this test ensures that our generated code won't relax the constraunsigneds on argument evaluation order, but on the contrary will strengthen them.

struct A {

	unsigned a;
	unsigned b;
	unsigned c;

	A() {	}
	A(unsigned a) {	}
	A(unsigned a, unsigned b) {	}
	A(unsigned a, unsigned b, unsigned c) : a(a), b(b), c(c) {	}
};

A returnsA() {
	return {};
}
A returnsA1() {
	return {1};
}

// guaranteed to be random
unsigned pseudoRand() {
	static unsigned i = 0;
	return i++;
}

int main() {

	// Before we had this feature, braced_init_list initialized objects were initialized with normal parenthesis. Calls like that then don't have a specified evaluation order
	A a1 = {};
	A a2 = {1};
	A a3 = {1, 2};

	A a4{};
	A a5{1};
	A a6{1, 2};

	A a7;
	A a8(1);
	A a9(1, 2);

	returnsA();
	returnsA1();


	// finally, here we will test, whether the order is actually not weakened, but stays the same
	A aOrder1 = {pseudoRand(), pseudoRand(), pseudoRand()};
	A aOrder2{pseudoRand(), pseudoRand(), pseudoRand()};
	A aOrder3(pseudoRand(), pseudoRand(), pseudoRand());

	std::cout << "aOrder1.a = " << aOrder1.a << " aOrder1.b = " << aOrder1.b << " aOrder1.c = " << aOrder1.c << std::endl;
	std::cout << "aOrder2.a = " << aOrder2.a << " aOrder2.b = " << aOrder2.b << " aOrder2.c = " << aOrder2.c << std::endl;

	// note that we can NOT compare the values for the object aOrder3. In the reference version the evaluation order is unspecified (and right-to-left on my system here),
	// while it is specified left-to-right in the insieme-generated version now, because we always use braced_init_list syntax for constructors now
	//std::cout << "aOrder3.a = " << aOrder3.a << " aOrder3.b = " << aOrder3.b << " aOrder3.c = " << aOrder3.c << std::endl;

	return (aOrder1.a == 0 && aOrder1.b == 1 && aOrder1.c == 2 && aOrder2.a == 3 && aOrder2.b == 4 && aOrder2.c == 5) ? 0 : 1;
}

