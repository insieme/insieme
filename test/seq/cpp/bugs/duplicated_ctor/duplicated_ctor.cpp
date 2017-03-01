#include <iostream>

class A {
public:
	int * p;
	A(int n) : p(new int()) { *p = 42; }
};

int main() {
	A a(3);
	std::cout << *a.p << std::endl;
	return 0;
}
