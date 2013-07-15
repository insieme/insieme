//
//
//
//
//
//
//
// currently not supported 
// would need some changes in frontend/backend
// example: add flag for union/struct to c_ast::structtype 
//
//
//
//
//
//
//
#include <stdio.h>
#include <iostream>

union U {
	U() { std::cout << "U()" << std::endl; }
	int i;
	char a;
	void member1() { printf("asdf %c\n", a); } 
	void member2() { std::cout << "asdf " << i << std::endl; } 
};

int main() {

	U u;
	u.a = 'a';
	u.member1();
	u.i = 0;
	u.member2();

	return 0;
}
