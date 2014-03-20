#pragma once
#include <iostream>
#include "crtp.h"
struct Base : public CRTP<Base*>{
	Base() { 
		std::cout << "Base()" << std::endl; 
	}
	virtual void loadArguments() = 0;
	virtual void run() = 0;
	virtual void runProc();
};
