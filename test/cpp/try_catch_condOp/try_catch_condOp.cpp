#include <iostream>

void throwFun() throw(int) {
	throw 1;
}

int throwFun1() throw(int) {
	throw 1;
	return 1;
}

int main() {

	try {
		int x = 10;
		x < 10 ? throw 10 : 1;
	} catch(int e) { std::cout << "exception caught " << e << std::endl; }

	try {
		int x = 10;
		x < 10 ? 1 : throw 10;
	} catch(int e) { std::cout << "exception caught " << e << std::endl; }

	try {
		int x = 10;
		x < 10 ? throw x : 1;
	} catch(int e) { std::cout << "exception caught " << e << std::endl; }

	try {
		int x = 10;
		x < 10 ? 1 : throw x;
	} catch(int e) { std::cout << "exception caught " << e << std::endl; }

    try { x < 10 ? throwFun1() : 1; } catch(int e) { std::cout << "exception caught " << e << std::endl; }
	try { x < 10 ? 1 : throwFun1(); } catch(int e) { std::cout << "exception caught " << e << std::endl; }

	try{ x < 10 ? throwFun1() : 1; } catch(...) { std::cout << "exception caught " << std::endl; }
	try{ x < 10 ? 1 : throwFun1(); } catch(...) { std::cout << "exception caught " << std::endl; }

	return 0;
}

