#include <stdio.h>

class Base {
public:
	Base() {  }
	~Base() {  }
	
	void g(){
		printf("papa\n");
	}
};

class Derived: public Base {
	int a;
public:
	Derived() {  }
	~Derived() {  }

	void f(){
		printf("child\n");
	}
};

class A {};
class B {};

int main() {

	//builtin types
	{
		int aInt = 1, bInt =2;
		long aLong = 3, bLong = 4;
		double aDouble = 5.0, bDouble = 5.1;

		aInt = bLong;		// implicit
		printf("%d\n", aInt);
		aInt = (int) bLong;	// c style
		printf("%d\n", aInt);
		aInt = int(bLong);	// functional
		printf("%d\n", aInt);

		aLong = bInt;			// implicit
		printf("%d\n", aLong);
		aLong = (long) bInt;	// c style
		printf("%d\n", aLong);
		aLong = long(bInt);		// functional
		printf("%d\n", aLong);

		aInt = bDouble;			// implicit
		printf("%d\n", aInt);
		aInt = (int) bDouble;	// c style
		printf("%d\n", aInt);
		aInt = int(bDouble);	// functional
		printf("%d\n", aInt);

		aDouble = bInt;				// implicit
		printf("%f\n", aDouble);
		aDouble = (double) bInt;	// c style
		printf("%f\n", aDouble);
		aDouble = double(bInt);		// functional
		printf("%f\n", aDouble);
	}

	//pointers cast
	{
		Base* pba;
		Base* pbb = new Base();
		pbb->g();

		Base* pbc = new Derived();			//implicit
		pbc->g();
		Base* pbd ;
		 pbd = (Base* ) new Derived();	//explicit c-style

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
