#pragma once
#include <iostream>
#include "base1.h"

struct Derived1 : public Base1 {
	//FIXME: if WITHOUT user-provided-ctor we miss the base call
	Derived1() {} //: Base1() {};
	//Derived1() : Base1() {};
	virtual void loadArguments(); 
	void run();
};
