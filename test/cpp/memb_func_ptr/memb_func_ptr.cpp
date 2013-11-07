#include <iostream>

class Obj{
	public:

		void f1 ( void) {std::cout << "f1" << std::endl;}
		void f2 ( void) {std::cout << "f2" << std::endl;}
		void f3 ( void) {std::cout << "f3" << std::endl;}
};


int main (){


	typedef void (Obj::*memb_ptr_t) ( void);
	memb_ptr_t ptr = &Obj::f1;


	Obj a;



	return 0;
}
