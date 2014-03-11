#pragma once
#include <iostream>
#include "base.h"

struct Derived : public Base {
	//FIXME: if WITHOUT user-provided-ctor we miss the base call
	//Derived() {} //: Base() {};
	//Derived() : Base() {};
	virtual void loadArguments(); 
	void run();
};
