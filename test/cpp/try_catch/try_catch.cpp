#include <iostream>

void throwFun() throw(int) {
	throw 1;
}

int throwFun1() throw(int) {
	throw 1;
	return 1;
}

struct Obj {
	Obj() : a(10) {}
	int a;
};

struct Obj1 {
	Obj1() : a(10) {}
	int a;
};

struct Obj2 {
	Obj2() : a(10) {}
//	~Obj2() { std::cout << "~Obj2() " << this->a << std::endl; }
	int a;
};

struct POD {
	int a;
};

int main() {

	try { throw 10; } catch(int e) { std::cout << "exception caught " << e << std::endl; }

	int x = 10;
	try { throw x; } catch(int e) { std::cout << "exception caught " << e << std::endl; }
	try{ throwFun(); } catch(int e) { std::cout << "exception caught " << e << std::endl; }
	try { x = throwFun1(); } catch(int e) { std::cout << "exception caught " << e << std::endl; }
	try{ throwFun(); } catch(...) { std::cout << "exception caught " << std::endl; }
	try{ x = throwFun1(); } catch(...) { std::cout << "exception caught " << std::endl; }

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

	{
		Obj2 a;
		try { throw a; } catch(Obj2 e) { std::cout << "exception caught " << e.a << std::endl; }
	}
	try { throw Obj2(); } catch(Obj2 e) { std::cout << "exception caught " << e.a << std::endl; }

	{
		Obj2 a;
		try { throw a; } catch(...) { std::cout << "exception caught" << std::endl; }
	}
	try { throw Obj2(); } catch(...) { std::cout << "exception caught" << std::endl; }

	{
		Obj2 a;
		try { throw a; } 
		catch(Obj2 e) { std::cout << "exception caught " << e.a << std::endl; }
		catch(...) { std::cout << "exception caught" << std::endl; }
	}
	{
		try { throw Obj2(); }
		catch(Obj2 e) { std::cout << "exception caught " << e.a << std::endl; }
		catch(...) { std::cout << "exception caught" << std::endl; }

		try { throw 1; }
		catch(Obj2 e) { std::cout << "exception caught " << e.a << std::endl; }
		catch(...) { std::cout << "exception caught" << std::endl; }
	}

	/*
	try { throw POD(); } catch(POD e) { std::cout << "exception caught " << e.a << std::endl; }
	*/

	try { POD o = {1}; throw o; } catch(POD e) { std::cout << "exception caught " << e.a << std::endl; }
	{
		POD o = {1};
		try { throw o; } catch(POD e) { std::cout << "exception caught " << e.a << std::endl; }
	}

	return 0;
}

