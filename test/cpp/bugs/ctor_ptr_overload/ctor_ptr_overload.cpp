#include <iostream>


class Obj {
	int a;


public:

	Obj (const int * x):  a(*x) {
		std::cout << " A "<< std::endl;
	}
	Obj (int * x):  a((*x)+1) {
		std::cout << " B "<< std::endl;
	}

};



int main (){

	int a;
	int * b = &a;
	const int * c = &a;

	Obj x(b);
	Obj y(c);

	return 0;
}
