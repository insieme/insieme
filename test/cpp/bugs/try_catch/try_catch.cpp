#include <iostream>

struct POD { int a; };

struct Obj {
	Obj() : a(10) {}
//	~Obj() { std::cout << "~Obj() " << this->a << std::endl; }
	int a;
};

Obj* fun1(Obj* o) { return o; }

int main() {

	/*
	{
		Obj* b = new Obj();
		try { throw fun1(b); } catch(Obj* e) { std::cout << "exception caught " << e->a << std::endl; }
		try { throw b; } catch(Obj* e) { std::cout << "exception caught " << e->a << std::endl; }

		Obj a;
		try { throw 0 ? &a : b; } catch(Obj* e) { std::cout << "exception caught " << e->a << std::endl; }
		try { throw new Obj(); } catch(Obj* e) { std::cout << "exception caught " << e->a << std::endl; delete e; }
	}

	
	{
		Obj a;
		try { throw a; } catch(Obj e) { std::cout << "exception caught " << e.a << std::endl; }

		try { throw Obj(); } catch(Obj e) { std::cout << "exception caught " << e.a << std::endl; }
	}
	

	{
		Obj a;
		try { throw a; } catch(Obj& e) { std::cout << "exception caught " << e.a << std::endl; }
		try { throw Obj(); } catch(Obj& e) { std::cout << "exception caught " << e.a << std::endl; }
	}
	*/

	Obj();
	Obj o;
	try { throw Obj(); } catch(Obj e) { std::cout << "exception caught " << e.a << std::endl; }
	POD();
	POD p;
	try { throw (POD){0}; } catch(POD e) { std::cout << "exception caught " << e.a << std::endl; }

	return 0;
}

