#include <iostream>
struct C {
	C() { std::cout << "C()" << std::endl; }
	C(const C& r) {  std::cout << "C(const C&)" << std::endl;}
	~C() { std::cout << "~C()" << std::endl; };

};

struct D{
	C* pc;
	void makeC1() { this->pc = new C(C());}
};
int main() {

	D d;
	d.makeC1();
	return 0;
}
