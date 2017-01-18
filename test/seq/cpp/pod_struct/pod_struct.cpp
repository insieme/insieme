#include <iostream>

struct POD {
	int a;
};

struct P : public POD {
	int b;
};

int main() {
	{
		POD t;
		t.a = 5;
		std::cout << "1: " << t.a << std::endl;
	}
	
	{
		POD p = {};
		std::cout << "2: " << p.a << std::endl;
	}

	{
		POD p = {1};
		std::cout << "3: " << p.a << std::endl;
	}
//
//	{
//		//TODO currently zero-initialized 
//		//default-init
//		POD *f = new POD;
//		delete f;
//		std::cout << "4: " << f->a << std::endl;
//	}
//
//	/*
//	{
//		//zero-init
//		POD *f = new POD();
//		std::cout << "5: " << f->a << std::endl;
//		delete f;
//	}
//	*/
//
//
//	{
//		POD x;
//		POD y(x);
//	}
//
//
//	{
//		int i = ((POD){10}).a;
//		std::cout << i << std::endl;
//		std::cout << "7: " <<  ((POD){10}).a << std::endl;
//		std::cout << "8: " << ((POD){10}).a << std::endl;
//	}
//
//	{
//		P p;
//	}
	return 0;
}
