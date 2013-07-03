#include <iostream>

struct Obj {
	Obj() : a(10) {}
	//std::cout needs global
	~Obj() { std::cout << "~Obj() " << this->a << std::endl; }
	int a;
};

int main() {
	Obj o;
}
