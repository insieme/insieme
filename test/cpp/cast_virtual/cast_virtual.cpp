#include <stdio.h>

class Base {
	virtual void dummy() {}
public:
	Base() {  }
	~Base() {  }
};

class Derived: public Base {
	int a;
public:
	Derived() {  }
	~Derived() {  }
};

class A {};
class B {};

int main() {

	//builtin types
	{
		int aInt, bInt;
		long aLong, bLong;
		double aDouble, bDouble;

		aInt = bLong;		// implicit
		aInt = (int) bLong;	// c style
		aInt = int(bLong);	// functional

		aLong = bInt;			// implicit
		aLong = (long) bInt;	// c style
		aLong = long(bInt);		// functional

		aInt = bDouble;			// implicit
		aInt = (int) bDouble;	// c style
		aInt = int(bDouble);	// functional

		aDouble = bInt;				// implicit
		aDouble = (double) bInt;	// c style
		aDouble = double(bInt);		// functional
	}

	//pointers cast
	{
		Base* pba;
		Base* pbb = new Base();

		Base* pbc = new Derived();			//implicit
		Base* pbd = (Base* ) new Derived();	//explicit c-style

		Derived* pda;
		Derived* pdb = new Derived();

		//derived to base cast
		pba = pdb;				//implicit
		pba = (Derived*) pdb;	//explicit
	}

	{
		Base b;
		Base& rb = b;

		Derived d;
		Derived& rd = d;

		//derived to base cast
		Base& rba = d;			//implicit
		Base& rbb = (Base&) d;	//explicit

		//base to derived cast
		Derived& r1 = (Derived&) b;	//explicit
	}

	//dynamic_cast <new_type> (expression)
	{
		Base* pba = new Derived();
		Base* pbb = new Base();
		Derived* pd;

		pd = dynamic_cast<Derived*>(pba);
		if (pd==0) printf("Null pointer on first type-cast\n");

		pd = dynamic_cast<Derived*>(pbb);
		if (pd==0) printf("Null pointer on second type-cast\n");
	}

	//reinterpret_cast <new_type> (expression)
	{
		A * a = new A;
		B * b = reinterpret_cast<B*>(a);
	}

	//static_cast <new_type> (expression)
	{
		Base * a = new Base;
		Derived * b = static_cast<Derived*>(a);
	}

	//const_cast <new_type> (expression)
	{
		int x = 1;
		const int* c = &x;
		int* y;

		y = const_cast<int *>(c);
		// y == 1
		printf("y 1 == %d\n", *y);
		(*y)++;
		// y == 2
		printf("y 2 == %d\n", *y);
	}

	return 0;
}
