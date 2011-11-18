class Base { virtual void dummy() {} };
class Derived: public Base { int a; };

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

	//base class cast
	{
		Base* pba;
		Base* pbb = new Base;

		Base* pbc = new Derived;			//implicit
		Base* pbd = (Base* ) new Derived;	//explicit c-style
		Base* pbd = Base*(new Derived);		//functional

		Derived* pdb;
		Derived* pda = new Derived;

		//derived to base cast
		pba = pdb;
	}

	//not working 17/11/11
	//dynamic_cast <new_type> (expression)
	{
		Base* pba = new Derived;
		Base* pbb = new Base;
		Derived* pd;

		pd = dynamic_cast<Derived*>(pba);
		if (pd==0) std::cout << "Null pointer on first type-cast" << std::endl;

		pd = dynamic_cast<Derived*>(pbb);
		if (pd==0) std::cout << "Null pointer on second type-cast" << std::endl;
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
		std::cout << "y 1 == " << y;
		(*y)++;
		// y == 2
		std::cout << "y 2 == " << y;
	}

	return 0;
}
