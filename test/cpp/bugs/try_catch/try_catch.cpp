#include <iostream>

struct Obj {
	Obj() : a(10) {}
//	~Obj() { std::cout << "~Obj() " << this->a << std::endl; }
	int a;
};

int main() {

	{
		try { throw new Obj(); } catch(Obj* e) { std::cout << "exception caught " << e->a << std::endl; delete e; }
	}	

	{
		Obj a;
		try { throw a; } catch(Obj e) { std::cout << "exception caught " << e.a << std::endl; }
	}
	try { throw Obj(); } catch(Obj e) { std::cout << "exception caught " << e.a << std::endl; }

	{
		Obj a;
		try { throw a; } catch(Obj& e) { std::cout << "exception caught " << e.a << std::endl; }
	}
	try { throw Obj(); } catch(Obj& e) { std::cout << "exception caught " << e.a << std::endl; }

	return 0;
}

