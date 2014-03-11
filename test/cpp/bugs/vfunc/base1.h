#pragma once
#include <iostream>
#include "crtp.h"
struct Base1 : public CRTP<Base1*>{
	Base1() { 
		std::cout << "Base1()" << std::endl; 
	}
	virtual void loadArguments() = 0;
	virtual void run() = 0;
	virtual void runProc();
};
