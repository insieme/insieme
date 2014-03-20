#include <iostream>

#include "header.h"

struct DUMMY;


typedef toIntercept::Obj<struct OTHER> Obj2;

int main (){

//	{
//	toIntercept::Obj<int> x(6);
//	std::cout << x.get() << std::endl;
//	}

	{
	toIntercept::Obj<DUMMY> x(43);
	std::cout << x.get() << std::endl;
	}

	{
	Obj2 x(1234);
	std::cout << x.get() << std::endl;
	}

	//{
	//Obj<struct ANOTHER> x(765);    /// << this is not alowed in cpp 03 fails even in gcc
	//std::cout << x.get() << std::endl;
	//}


	return 0;
}
