#include <cstdio>
#include <iostream>

class Base {
public:
	virtual void dummy() { }
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
		{
			Derived* pdd = new Derived();

			//derived to base cast
			Base* pbd = (Base*) new Derived();	//explicit c-style
			Base* pbd1 = new Derived();			//implicit
			Base* pbd2;
			pbd2 = pdd;							//implicit
			
			delete pdd;
			delete pbd;
			delete pbd1;
		}
		
		//base to derived
		{
			Derived* pda;
			Base* pbb = new Base();
			pda = (Derived*) pbb;	//explicit
			Derived* pda1 = (Derived*) pbb;	//explicit

			delete pbb;
		}
	}
	{
		//derived to base cast
		{
			Derived d;
			Derived& rd = d;

			Base& rba = d;			//implicit
			Base& rbb = (Base&) d;	//explicit
		}

		//base to derived cast
		{
			Base b;
			Base& rb = b;
			Derived& r1 = (Derived&) b;	//explicit
		}
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

		{
			Derived* pd = dynamic_cast<Derived*>(pba);
			if (pd==0) printf("Null pointer on first type-cast\n");
		}

		{
			Derived* pd = dynamic_cast<Derived*>(pbb);
			if (pd==0) printf("Null pointer on second type-cast\n");
		}
	}
	{
		Derived d;
		Derived& rd = d;
		Base b;
		Base& rb = b;

		Base& rb1 = dynamic_cast<Derived&>(rd);

		/* need exceptions for following code
		try{ 
			Base& rb1 = dynamic_cast<Derived&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		
		try{ 
			Derived& rd1 = dynamic_cast<Derived&>(rb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		*/
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
