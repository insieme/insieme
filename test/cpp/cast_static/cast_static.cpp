#include <cstdio>
#include <iostream>

class Base {
public:
	virtual void dummy() { std::cout << "Dummy" << std::endl; }
	Base() {  }
	//~Base() {  }
};

class Derived: public Base {
	int a;
public:
	Derived() {  }
	//virtual void dummy() { std::cout << "Dervied" << std::endl; }
	//~Derived() {  }
};

struct A {
	int a;
};

struct B {
	int b;
};

int main() {

	{
 		Derived d;
		Base b;
		Base* pb = &b;
		Derived* pd = &d;

		Base* pb1 =  static_cast<Base*>(pd);
		pb1->dummy();

		Derived* pd1 =  static_cast<Derived*>(pb);
		pd1->dummy();
	}

	{
		Base b;
		Base& rb = b;
 		Derived d;
		Derived& rd = d;
		Base& rbd1 = static_cast<Base&>(rd);
		rbd1.dummy();

		Derived& rdb1 = static_cast<Derived&>(rb);
		rdb1.dummy();
	}

	return 0;
}
