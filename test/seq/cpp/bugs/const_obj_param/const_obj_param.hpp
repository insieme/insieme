#include <iostream>

namespace NS {

struct S {
	int m;
	S(int i) : m(i) { }
	void mFunc_constPar(const S other) {
		std::cout << "other: " << other.m << std::endl;
	}
	void mFunc_nonConstPar(S other) {
	
		std::cout << "other: " << other.m << std::endl;
		const int& rm = other.m;
		std::cout << "other: " << rm << std::endl;
	}
	
	void mFunc_nonConstPar1(S other) {
		
		std::cout << "other: " << other.m << std::endl;
		other.m = 0;
		std::cout << "other: " << other.m << std::endl;
	}
};

}
