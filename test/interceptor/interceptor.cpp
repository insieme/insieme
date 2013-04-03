#include<fun.h>
#include<class.h>
#include<stdio.h>

using namespace ns;

struct B {
	int x;
	B(int x = 12) : x(x) {}
	int f() { return x; }
};

int main() {

	// an intersected function
	printf("%d\n", func(0));

	// an intersected type
	A a;
	printf("%d\n", a.f(4));

	// a non-intersected type
	B b;
	printf("%d\n", b.f());

	return 0;
}
