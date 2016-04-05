#include <iostream>

struct A {
	struct {
		int a;
		float b;
	};
	A(int val) 
		: a(val)
	{
	}
};

int main(){
	A x(42);
	std::cout << x.a << std::endl;
	return 0;
}
