#include "header.hpp"
#include <iostream>

void H::setA(int a) {
	mA = a;
}

int H::getA() {
	return mA;
}

int main() {
	H h(1);
	std::cout << "1 == " << h.getA();
	h.setA(10);
	std::cout << "10 == " << h.getA();

	return 0;
}
