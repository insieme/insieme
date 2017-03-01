#include <iostream>

/**
 * This test case ensures that recursive types with initializer
 * lists are working (bug found within some project code).
 */

struct A {

	A* next;
	int i;

	A(int i=0) : i(i), next(0) {}
};

int main() {
	
	A a;
	A b(1);

	std::cout << a.i << " | " << b.i << std::endl;
	
	return 0;
}
