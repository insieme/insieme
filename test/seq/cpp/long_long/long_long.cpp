
#include <iostream>
#include<stdint.h>

struct C {

	int f(signed char x) {
		return 1;
	}

	int f(short int x) {
		return 2;
	}

	int f(int x) {
		return 3;
	}

	int f(long x) {
		return 4;
	}

	int f(long long y) {
		return 5;
	}


	int f(char x) {
		return 6;
	}

	int f(unsigned char x) {
		return 7;
	}
};


int main() {


	C o;

	char x;
	unsigned char y;

	o.f(x);
	o.f(y);

	signed char 	a = 0;
	short int 	b = 0;
	int 		c = 0;
	long int 	d = 0;
	long long int 	e = 0;
	

	std::cout << "signed char: " << sizeof(a) << " - " << o.f(a) << "\n";
	std::cout << "short int:   " << sizeof(b) << " - " << o.f(b) << "\n";
	std::cout << "int:         " << sizeof(c) << " - " << o.f(c) << "\n";
	std::cout << "long:        " << sizeof(d) << " - " << o.f(d) << "\n";
	std::cout << "long long:   " << sizeof(e) << " - " << o.f(e) << "\n";

	char f = 'a';
	unsigned char g = 0;

	std::cout << "char:          " << sizeof(f) << " - " << o.f(f) << "\n";
	std::cout << "unsigned char: " << sizeof(g) << " - " << o.f(g) << "\n";

	return 0;
}
