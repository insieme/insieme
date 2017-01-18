#include <iostream>
#include "const_obj_param.hpp"

void f_const(const NS::S s) {
	std::cout << "f const " << s.m << std::endl;
	const int& rm = s.m;
	std::cout << "f const " << rm << std::endl;
}

void f_nonConst(NS::S s) {
	std::cout << "f non const " << s.m << std::endl;
	s.m = 100;
	std::cout << "other: " << s.m << std::endl;
}
int main() {
	using namespace NS;
	S s1(90);
	S s2(10);

	s1.mFunc_constPar(s2);
	s1.mFunc_nonConstPar(s2);
	s1.mFunc_nonConstPar1(s2);

	f_const(s1);
	f_nonConst(s1);

	return 0;
}
