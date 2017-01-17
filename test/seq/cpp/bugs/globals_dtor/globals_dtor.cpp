#include <iostream>

int global; 

struct Obj {
	Obj() : a(10) {}
	//std::cout needs global
	~Obj() { std::cout << "~Obj() " << this->a << " " << global << std::endl; }
	int a;
};

int main() {
	Obj o;
}
