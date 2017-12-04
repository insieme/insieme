#include <iostream>

int x = 0;
struct S
{
	int n = ++x;
	S() { }                 // uses default member initializer
	S(int arg) : n(arg) { } // uses member initializer list
};

int main()
{
	std::cout << x << '\n'; // prints 0
	S s1;
	std::cout << x << '\n'; // prints 1 (default initializer ran)
	S s2(7);
	std::cout << x << '\n'; // prints 1 (default initializer did not run)
}
