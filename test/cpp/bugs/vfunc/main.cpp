#pragma once
//#include <iostream>
//#include <vector>
#include "derived.h"
#include "base.h"

int main() {
	Base* b = new Derived();
	b->runProc();
	delete b;
	//new Derived();
	//(new Derived())->runProc();
	//std::vector<Derived> v;

	//if(!v.empty())  
	//	v[0].runProc();
}
