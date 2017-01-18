#include <iostream>
struct EmptyPOD {
	void f() { std::cout << "f" << std::endl; };
};
int main() {
	EmptyPOD e;
	e.f();
	return 0;
}
