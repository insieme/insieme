
#include <iostream>

// Tests the correct implementation of default initialized members in the absence of ctors

struct Trivial {
	int i;
	Trivial() {
		std::cout << "Trivial::ctor" << std::endl;
	}
	Trivial(int i) : i(i) {
		std::cout << "Trivial::ctor(int)" << std::endl;
	}
};

struct Base {
	Base() {
		std::cout << "Base::ctor" << std::endl;
	}
};

Trivial getTrivial(int i) {
	std::cout << "getTrivial" << std::endl;
	return {i};
}

struct NoInit : Base {
	int i;
	Trivial t;
};

struct DefaultInit : Base {
	int i = 5;
	Trivial t1{6};
	Trivial t2 = getTrivial(7);
};


int main() {
	std::cout << "begin" << std::endl;
	NoInit ni;
	std::cout << "mid" << std::endl;
	DefaultInit di;
	std::cout << "di.i " << di.i << std::endl;
	std::cout << "di.t1.i " << di.t1.i << std::endl;
	std::cout << "di.t2.i " << di.t2.i << std::endl;
	std::cout << "end" << std::endl;
}
