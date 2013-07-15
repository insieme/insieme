#include <iostream>


#include "class.h"

extern int a;

int main(){

	A g(100);
	std::cout << "value of class static" << A::z << std::endl;
	A f(101);
	std::cout << "value of class static" << A::z << std::endl;
	A h(102);
	std::cout << "value of class static" << A::z << std::endl;

	std::cout << "extern" << a << std::endl;

	std::cout << "statics " << A::zcall << " : " << A::acall << std::endl;

	return 0;
};



