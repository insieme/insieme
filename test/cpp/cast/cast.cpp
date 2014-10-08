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
		printf("%ld\n", aLong);
		aLong = (long) bInt;	// c style
		printf("%ld\n", aLong);
		aLong = long(bInt);		// functional
		printf("%ld\n", aLong);

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

    //volatile
    {
        volatile int aInt = 1;
        bool aBool;
        double aDouble;
        
        aBool = aInt;
		printf("%d\n", aBool);
        aBool = (bool)aInt;
		printf("%d\n", aBool);
        aBool = bool(aInt);
		printf("%d\n", aBool);

        aDouble = aInt;
		printf("%f\n", aDouble);
        aDouble = (double)aInt;
		printf("%f\n", aDouble);
        aDouble = double(aInt);
		printf("%f\n", aDouble);
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
		{
			Derived d;
			Base ((Base)d);
		}


		//derived to base cast
		{
			Derived d;
			Derived& rd = d;

			Base& rba = d;			//implicit
			Base& rbb = (Base&) d;	//explicit

			Base& rbc = rd;
			Base& rbd = (Base&)rd;

		}

		//base to derived cast
		{
			Base 		b;
			Base& 		rb  = b;
			const Base& crb = b;

			Derived& r1 = (Derived&) b;	//explicit
			Derived& r2 = (Derived&) rb;
			//Derived& r3 = (Derived&) crb;  // this is allowed in C++, enforce cast, but i dont want to model it in IR, 
			const Derived& r3 = (Derived&) crb;

			//Derived& r3 = b;     // implicit in this case is not allowed
			//Derived& r4 = rb;
		}
	}

//	// INVALID -- needs polymorphic classes -- see cast_virtual
	//dynamic_cast <new_type> (expression)
	//{
//		Base* pba = new Derived();
//		Base* pbb = new Base();
//		Derived* pd;
//
//		pd = dynamic_cast<Derived*>(pba);
//		if (pd==0) printf("Null pointer on first type-cast\n");
//
//		pd = dynamic_cast<Derived*>(pbb);
//		if (pd==0) printf("Null pointer on second type-cast\n");
//	}


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
