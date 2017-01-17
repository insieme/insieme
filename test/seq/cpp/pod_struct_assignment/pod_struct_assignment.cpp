
#include <iostream>

using namespace std;

struct A {
	int x; 
};

int main() {

	A a;
	a.x = 12;

	// copy it
	A b = a;

	// a and b should not be alias
	cout << ((&a == &b)?"alias":"distinct") << "\n";

	// should have the same value
	cout << "a.x = " << a.x << "\n";
	cout << "b.x = " << b.x << "\n";

	// update one
	b.x = 14;
	cout << "a.x = " << a.x << "\n";
	cout << "b.x = " << b.x << "\n";

	return 0;
}
