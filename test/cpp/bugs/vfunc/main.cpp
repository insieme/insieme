#pragma once
#include <iostream>
#include <vector>
#include "derived.h"
#include "base.h"

int main() {
	Base* b = new Derived();
	b->runProc();
	//std::vector<Derived> v;

	//if(!v.empty())  
	//	v[0].runProc();
}
