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
		Derived& rd = d;
		Base b;
		Base& rb = b;

		Base& rbb = b;
		Base& rbd = d;

		/*
		try{ 
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rbd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		try{ 
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rbb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		try{ 
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}

		try{ 
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Base&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		*/
		
		// need exceptions for following code
		/*
		try{ 
			std::cout << "Derived to base" << std::endl;
			Base& rb1 = dynamic_cast<Derived&>(rd);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		*/
		
		///*
		try{ 
			std::cout << "Base to derived" << std::endl;
			Derived& rd1 = dynamic_cast<Derived&>(rb);
			//Derived& rd1 = rd;
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		//*/

		/*
		try{ 
		//INVALID
			Derived& rd1 = dynamic_cast<Base&>(rb);
		} catch(std::exception& e) {
			std::cout << e.what() << std::endl;
		}
		*/
	}
	
	return 0;
}
