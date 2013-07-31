#include <iostream>

struct Obj {
	static int STATIC;
	Obj() : a(10) {}
	//std::cout needs global
	~Obj() { std::cout << "~Obj() " << this->a << " " << std::endl; }
	int a;
};

int Obj::STATIC = 16;
int main() {
	Obj o;
	Obj::STATIC--;
}
