
#include <iostream>
#include "class.h"

int A::z = 10;

A::A(int v) {
	static int firstCtor = v;
	std::cout << "firstCtor: " << firstCtor << std::endl;
	z++;
}

int a = A::z;

int f(int a){
	std::cout << "initialize " << a << std::endl;
	return a;
}


int A::zcall = f(10);
int A::acall = f(11);
