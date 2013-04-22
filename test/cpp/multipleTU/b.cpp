#include <stdio.h>

#include "a.h"

/**
 * this test makes use of a class to instantiate objects
 * in two different translation units
 */

Obj::Obj(int i)
	: x(i)
{ }

int Obj::f(){
	return 0;
}


int main(int argc, char** argv) {
	Obj o(15);
	int res = f(o.f());
	printf("15=%d\n", res);
}
