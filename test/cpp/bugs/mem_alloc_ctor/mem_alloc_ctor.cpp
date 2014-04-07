#include <iostream>
struct C {
	int value;
	C() : value(123456) {}
	C(const C& o) : value(o.value) {}
	~C() {};

};

struct D{
	C* pc;
	void makeC1() { this->pc = new C(C());}
};
int main() {

	D d;
	d.makeC1();
	std::cout << " d.pc = " << d.pc->value << std::endl;
	return 0;
}
