#include <iostream>

struct S {
	int m;
	S(int x=0):m(x) { std::cout << "S()" << std::endl; }
	S(const S& o) : m(o.m) { std::cout << "S(const S&)" << std::endl; }
	S& operator= (S o) { m=o.m; std::cout << "operator=(S)" << std::endl; return *this; }
};

//NO DTOR -- 
struct C {
	int m;
	C(int x=0):m(x) { std::cout << "C()" << std::endl; }
	C(const C& o) : m(o.m) { std::cout << "C(const C&)" << std::endl; }
	C& operator= (C o) { m=o.m; std::cout << "operator=(C)" << std::endl; return *this; }
};

int f() { return 10; }
const int& f(const int & i) { return i; }

int main() {
	{
		S s;
		const S &rs = s;
		const S cs;
		const S cs1 = rs;
	}
	{
		C c;
		const C &rc = c;
		const C cc;
		const C cc1 = rc;
	}
	{
		const int i = 0;
		const int x = f();
		const int y = f(i);
	}
}
