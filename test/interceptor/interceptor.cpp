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

	// an intercepted function
	printf("%d\n", func(0));
	printf("%d\n", ns::func(0));
	
	// an intercepted type
	A a;
	printf("%d\n", a.f(4));

	// a non-intercepted type
	B b;
	printf("%d\n", b.f());

	return 0;
}
