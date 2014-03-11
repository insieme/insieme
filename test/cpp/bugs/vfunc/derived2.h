#pragma once
#include <iostream>
#include "base2.h"

struct Derived2 : public Base2 {
	//FIXME: if WITHOUT user-provided-ctor we miss the base call
	//Derived2() {} //: Base2() {};
	Derived2() : Base2() {};
	virtual void loadArguments(); 
	void run();
};
