#include <iostream>


int f() {
	std::cout << "call" << std::endl;
	return 4;
}


struct Obj {
	Obj(int x = 0) {
		f();
		mf();
		mf2();
		mf3();
		if(x == 0) {
			std::cout << "def" << std::endl;
			return;   // this is the thing of this test
		}
		else {
			std::cout << "non def" << std::endl;
		}
	}
	Obj mf() {
		std::cout << "ret this " << std::endl;
		return *this;
	}
	Obj* mf2() {
		std::cout << "ret ptr this " << std::endl;
		return this;
	}
	Obj& mf3() {
		std::cout << "ret ref this " << std::endl;
		return *this;
	}
};


int main() {

	Obj a;
	Obj b(456);

	return 0;
}
