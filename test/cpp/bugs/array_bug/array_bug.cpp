#include <iostream>
//#include <array>
#include <tr1/array>
//#include <boost/array.hpp>

namespace CGAL {
namespace cpp0x {
	//using std::array;
	using std::tr1::array;
	//using boost::array;
}
}

template< typename T, typename... Args >
inline
CGAL::cpp0x::array< T, 1 + sizeof...(Args) >
make_array(const T & t, const Args & ... args)
{
  CGAL::cpp0x::array< T, 1 + sizeof...(Args) > a = { { t, static_cast<T>(args)... } };
  return a;
}

//#include <CGAL/array.h>

/*
struct D;

struct C {
	D* pd;
	int x;
	C() : x(0) {}
	C(const C& c) : pd(c.pd), x(c.x) {}
	C(int _x) : x(_x) {}
};

struct D{ 
	C* pc; 
	D() {}
};

struct X{
	CGAL::cpp0x::array<int, 3> a;
	X(int x, int y, int z) : a(CGAL::make_array(x,y,z)) {}
	X(const X& x) : a(x.a) {}
};

struct Y{
	CGAL::cpp0x::array<C, 3> c;
	Y() : c(CGAL::make_array(C(0), C(1), C(2))) { c[0];}
	Y(const Y& y) : c(y.c) {}
};

struct S2 {
	typedef int M;
	typedef CGAL::cpp0x::array<M,2> Ar;
	Ar m;
	S2 () {}
	S2 (const M&a) : m ((Ar) { { a, a } }) {}
};
*/

struct T {
	typedef int M;
	typedef CGAL::cpp0x::array<M,2> Ar;
	Ar x;
};

struct A { int x; };
int main() {

	A a1;
	A a2 = {1};
	CGAL::cpp0x::array<int,3> arr0 = make_array(1, 2, 3);
	CGAL::cpp0x::array<int,3> arr1 = { { 1, 2, 3 } };
	CGAL::cpp0x::array<int,3> arr2 = { { 0 } };
//	CGAL::cpp0x::array<int,3> arr3 = { { } };
	/*
	//CGAL::cpp0x::array<int,3> arr4 = { };
	X x1(1,2,3);
	Y y;
	S2 s;
	S2 s2(s);
	*/

	T t0 = { { { 1,2} } };
	T t1 = { make_array(1, 1) };
	T t2 = { 0 };
	T t3 = { };
	T t4;
	return 0;
}
