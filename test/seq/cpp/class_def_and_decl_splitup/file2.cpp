#include <iostream>
#include "class.h"

int f(int a){ return a; }

CLASS::CLASS(int v) {
	int i=f(1);
	std::cout << "CLASS::CLASS " << i << std::endl;
}
	
CLASS::~CLASS() { std::cout << "CLASS::~CLASS" << std::endl;}

void CLASS::m() { std::cout << "CLASS::m" << std::endl;}

