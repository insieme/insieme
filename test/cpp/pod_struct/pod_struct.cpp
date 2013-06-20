#include <iostream>

struct POD {
	int a;
};

int main() {
	{
		POD t;
		std::cout << t.a << std::endl;
	}
	
	/*
	{
		//TODO currently zero-initialized 
		POD p = {};
		std::cout << p.a << std::endl;
	}
	*/

	{
		POD p = {1};
		std::cout << p.a << std::endl;
	}

	{
		//TODO currently zero-initialized 
		//default-init
		POD *f = new POD;
		delete f;
		std::cout << f->a << std::endl;
	}

	/*
	{
		//zero-init
		POD *f = new POD();
		std::cout << f->a << std::endl;
		delete f;
	}
	*/


	{
		POD x;
		POD y(x);
	}


	{
		int i = ((POD){10}).a;
		std::cout << i << std::endl;
		std::cout << ((POD){10}).a << std::endl;
		std::cout << ((POD){10}).a << std::endl;
	}
	return 0;
}
