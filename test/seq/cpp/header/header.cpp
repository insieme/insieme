#include "header.hpp"
#include <stdio.h>

void H::setA(int a) {
	mA = a;
}

int H::getA() {
	return mA;
}

int main() {
	H h(1);
	printf("1 == %d\n", h.getA());
	h.setA(10);
	printf("10 == %d\n", h.getA());

	return 0;
}
