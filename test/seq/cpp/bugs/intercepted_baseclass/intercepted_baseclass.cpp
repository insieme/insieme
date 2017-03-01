#include <iostream>
#include <tr1/array>

struct Obj { Obj() { std::cout << "Obj()" << std::endl; } };

struct X : public std::tr1::array<int,3> {
	//X() : std::tr1::array<int,3>()  {}
};

struct Y {
	//Y() : std::tr1::array<int,3>()  {}
	std::tr1::array<int,3> m;
	virtual void y() {};
};

struct Z {
	//Z() : std::tr1::array<int,3>()  {}
	std::tr1::array<Obj,3> m;
	virtual void z() {};
};

struct Z1 : public std::tr1::array<Obj, 3> {
	Z1() : std::tr1::array<Obj,3>() { std::cout << "Z1()" << std::endl; }
};

struct Z2 : public std::tr1::array<Obj, 3> {
	//Z2() : std::tr1::array<Obj,3>()  {}
};

int main() {
	
	//X x;
	//x.m.size();

	X x;

	Y y;
	y.m.size();

	Z z;

	Z1 z1;
	Z2 z2;

	return 0;
}
