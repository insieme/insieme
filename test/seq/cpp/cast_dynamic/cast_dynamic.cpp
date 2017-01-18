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
	//pointers cast
	/*
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
	*/
	/*
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
	*/

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
		try{ 
			std::cout << "Derived to base" << std::endl;
			Derived d;
			Base& rbd = d;
			Base& rb1 = dynamic_cast<Derived&>(rbd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		try{ 
			std::cout << "Derived to base" << std::endl;
			Base b;
			Base& rbb = b;
			Base& rb1 = dynamic_cast<Derived&>(rbb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		try{ 
			Derived d;
			Derived& rd = d;
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		try{ 
			Derived d;
			Derived& rd = d;
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Base&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		
		// need exceptions for following code
		try{ 
			Derived d;
			Derived& rd = d;
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		
		try{ 
			std::cout << "Base to derived" << std::endl;
			Base b;
			Base& rb = b;
			Derived& rd1 = dynamic_cast<Derived&>(rb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		try{ 
			std::cout << "Base to derived" << std::endl;
			Base b;
			Derived& rd1 = dynamic_cast<Derived&>(b);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		// cast const_cpp_ref to const_cpp_ref
		try{ 
			std::cout << "Base to derived" << std::endl;
			Base b;
			Base& rb = b;
			const Base& crb = rb;
			const Derived& rd1 = dynamic_cast<const Derived&>(crb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		// cast cpp_ref to const_cpp_ref
		try{ 
			std::cout << "Base to derived" << std::endl;
			Base b;
			Base& rb = b;
			const Derived& rd1 = dynamic_cast<const Derived&>(rb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		/*
		try{ 
		//INVALID
			Base b;
			Base& rb = b;
			Derived& rd1 = dynamic_cast<Base&>(rb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		*/	
	}
	
	return 0;
}
