#pragma once
#include <iostream>
#include "crtp.h"
struct Base2 : public CRTP<Base2*>{
	Base2() { 
		std::cout << "Base2()" << std::endl; 
	}
	virtual void loadArguments() = 0;
	virtual void run() = 0;
	virtual void runProc();
};
