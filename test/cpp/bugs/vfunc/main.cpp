//#include <iostream>
//#include <vector>
#include "derived.h"
#include "derived1.h"
#include "derived2.h"
#include "base.h"
#include "base1.h"
#include "base2.h"

int main() {
	{
		//currently failing -- no user-provided default ctor in derived -- we miss the base ctor
		//call
		Base* b = new Derived();
		b->runProc();
		delete b;
	}
	{
		Base1* b = new Derived1();
		b->runProc();
		delete b;
	}
	{
		Base2* b = new Derived2();
		b->runProc();
		delete b;
	}

	{
		//delete (new Derived());
		//delete new Derived1();
		//delete new Derived2();
	}
	
	{
		//(new Derived())->runProc();
		//(new Derived1())->runProc();
		//(new Derived2())->runProc();
	}
}
