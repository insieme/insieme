#include <iostream>

struct Obj {
	static int STATIC;
	Obj() {}
	~Obj() { std::cout << "~Obj() " << STATIC << " " << std::endl; }
};

int Obj::STATIC = 16;
int main() {
	Obj o;
	Obj::STATIC--;
}
