#include <iostream>
template <typename T>
struct Obj{

	T& agregate;

	Obj(T& init)
	: agregate(init){
	}
};


Obj<int> f(int& v){
	return Obj<int>(v);
}


template <typename T>
struct A{

	T value;

	A(T init) : value(init) {}

};

template <typename T>
struct B{

	A<T> g() const{
		return A<T>(T());
	}

};

//NO DTOR -- 
struct C {
	int m;
	C(int x=0):m(x) { std::cout << "C()" << std::endl; }
	C(const C& o) : m(o.m) { std::cout << "C(const C&)" << std::endl; }
	C& operator= (C o) { m=o.m; std::cout << "operator=(C)" << std::endl; return *this; }
};

//DTOR -- results in expr-with-cleanups
//which can introduce materialize operations
struct S {
	int m;
	S(int x=0):m(x) { std::cout << "S()" << std::endl; }
	S(const S& o) : m(o.m) { std::cout << "S(const S&)" << std::endl; }
	S& operator= (S o) { m=o.m; std::cout << "operator=(S)" << std::endl; return *this; }
	~S() { std::cout << "~S()" << std::endl; }
};

const S& cr_cr_f(const S& s) { return s;}
//S& r_cr_f(const S& s) { return s;}
const S& cr_r_f(S& s) { return s;}
S& r_r_f(S& s) { return s;}

S s_s_fS(S s) {return s;}
C s_s_fC(C s) {return s;}
S s_cs_fS(const S s) {return s;}
C s_cs_fC(const C s) {return s;}

const S cs_cs_fS(const S s) {return s;}
const S cs_s_fS(S s) {return s;}

const C cs_cs_fC(const C s) {return s;}
const C cs_s_fC(C s) {return s;}

int main (){

	{
		int x = 8;
		const Obj<int>& o = f(x);
	}
	{
		B<int> b;
		const A<int>& ref = b.g();
	}
	{
		typedef A<float> aliasA;
		typedef B<float> aliasB;

		aliasB b;
		const aliasA& ref = b.g();
	}

	//without template
	{
		S s;
		const S &rs1 = s;
	}
	{
		//EXPR WITH CLEANUP
		const S &rs2 = S();
	}
	{
		//FIXME expects const_cpp_ref gets refstruct	
		//NO EXPR WITH CLEANUP
		//const C &rc1 = C();
	}
	{
		S s;
		const S &rs1 = s;
		const S &rs3 = S(rs1);
		S &rs2 = s;
		const S &rs4 = cs_cs_fS(s);
		const S &rs5 = cs_s_fS(s);
		const S &rs6 = s_s_fS(s);
		const S &rs7 = s_cs_fS(s);

	}
	{
		C c;
		const C &rc1 = c;
		//FIXME expects const_cpp_ref gets refstruct	
		//const C &rc3 = C(rc1);
		C &rs2 = c;
		const C &rc4 = cs_cs_fC(c);
		const C &rc5 = cs_s_fC(c);
		const C &rc6 = s_s_fC(c);
		const C &rc7 = s_cs_fC(c);

	}

	return 0;
}


