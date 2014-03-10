#pragma once
#include <iostream>
#include "base.h"

struct Derived : public Base {
	virtual void loadArguments(); 
	void run();
};
