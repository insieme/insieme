#include <iostream>


union UN{
	int   value1;
	float value2;

	bool func(int x){
		return x == value1;
	}
};


int main (){

	UN o;

	o.value2 = 0.0f;
	
	if (o.func(0))
		std::cout << "int and float have same 0" << std::endl;
	else
		std::cout << "int and float have different 0" << std::endl;

	return 0;
};
