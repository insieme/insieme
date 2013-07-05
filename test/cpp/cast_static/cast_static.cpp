#include <cstdio>
#include <iostream>

class Base {
public:
	virtual void dummy() { }
	Base() {  }
	//~Base() {  }
};

class Derived: public Base {
	int a;
public:
	Derived() {  }
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
		Derived* pd1 =  static_cast<Derived*>(pb);
	}

	{
		Base b;
		Base& rb = b;
 		Derived d;
		Derived& rd = d;
		Base& rbd1 = static_cast<Base&>(rd);
		Derived& rdb1 = static_cast<Derived&>(rb);
	}

	return 0;
}
