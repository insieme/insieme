
#include <iostream>
#include <boost/thread.hpp>


struct Obj {
	int a;
	float b;
};



// builtin
int buDef;
int buInit = 0;

// struct
Obj classDef;
Obj classInit = {1, 0.0};


// intercepted
boost::mutex mut;




int main (){

	{
		std::cout << buDef <<  std::endl;
		std::cout << buInit <<  std::endl;
	}

	{
		std::cout << classDef.a << classDef.b << std::endl;
		std::cout << classInit.a << classInit.b << std::endl;
	}

	{
		boost::lock_guard<boost::mutex> l(mut);
	}

	return 0;
}

