
#include <iostream>
#include <boost/thread.hpp>

struct A{
	int a;
	int b;

	A (int x = 0, int y = 0) : a(x), b(y) {}
};

class Obj {

	int field1;
	A x;
	boost::mutex mut;
	A y;
	int field2;

public:

	Obj() : x(), y(1,1) {
		field1 = 15;
		field2 = 32;
	}

	void mf()  {
		boost::lock_guard<boost::mutex> l(mut);
		std::cout << "a " << field1 << std::endl;
		std::cout << "b " << field2 << std::endl;
		std::cout << "c " << x.a << "," << x.b << std::endl;
		std::cout << "d " << y.a << "," << y.b << std::endl;
	}

};

int main (){

	Obj x;
	x.mf();

	return 0;
}

