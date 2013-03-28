#include<fun.h>
#include<class.h>

using namespace ns;

struct B {
	int x;
	B(int x = 12) : x(x) {}
	int f() { return x; }
};

int main() {

	// an intersected function
	func(0);

	// an intersected type
	A a;
	a.f(4);

	// a non-intersected type
	B b;
	b.f();

	return 0;
}
