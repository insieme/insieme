#include <iostream>

#include "temp.h"

int global = 1000;
int main (){

	global++;
	{
		float r = NS::f<int, 5+5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = NS::f<long long, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = NS::f<char, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = NS::f<bool, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = NS::f<int, 3, float> (7);
		std::cout << "r: " << r << std::endl;
	}


	return 0;
}
