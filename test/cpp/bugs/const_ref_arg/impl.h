#include <iostream>
struct S {
	int m;
	S(int x=0):m(x) { std::cout << "S()" << std::endl; }
	S(const S& o) : m(o.m) { }//std::cout << "S(const S&)" << std::endl; }
	S& operator= (S o) { m=o.m; std::cout << "operator=(S)" << std::endl; return *this; }
	S f() const;
	S operator()() const { return S();};
};


