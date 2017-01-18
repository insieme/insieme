#include <iostream>
template <typename T>
struct Obj{

	T& agregate;

	Obj(T& init)
	: agregate(init){
	}
};


Obj<int> f(int& v){
	std::cout << " f: " << v << std::endl;
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
	C(int x=0):m(x) {  std::cout << "C()" << std::endl; }
	C(const C& o) : m(o.m) {  std::cout << "C(const C&)" << std::endl; }
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

const S& cr_cr_f(const S& s) { 
	std::cout << " cr_cr_f: " << s.m << std::endl;
	return s;
}
const S& cr_r_f(S& s) { 
	std::cout << " cr_r_f: " << s.m << std::endl;
	return s;
}
S& r_r_f(S& s) { 
	std::cout << " r_r_f: " << s.m << std::endl;
	return s;
}

S s_s_fS(S s) {
	std::cout << " s_s_fS: " << s.m << std::endl;
	return s;
}
C s_s_fC(C s) {
	std::cout << " s_s_fC: " << s.m << std::endl;
	return s;
}
S s_cs_fS(const S s) {
	std::cout << " s_s_fS: " << s.m << std::endl;
	return s;
}
C s_cs_fC(const C s) {
	std::cout << " s_cs_fC: " << s.m << std::endl;
	return s;
}

const S cs_cs_fS(const S s) {
	std::cout << " cs_cs_fS: " << s.m << std::endl;
	return s;
}
const S cs_s_fS(S s) {
	std::cout << " cs_s_fS: " << s.m << std::endl;
	return s;
}

const C cs_cs_fC(const C s) {
	std::cout << " cs_cs_fC: " << s.m << std::endl;
	return s;
}
const C cs_s_fC(C s) {
	std::cout << " cs_s_fC: " << s.m << std::endl;
	return s;
}

int main (){

	{
		std::cout << " test 1 " <<std::endl;
		int x = 8;
		const Obj<int>& o = f(x);
	}
	{
		std::cout << " test 2 " <<std::endl;
		B<int> b;
		const A<int>& ref = b.g();
	}
	{
		std::cout << " test 3 " <<std::endl;
		typedef A<float> aliasA;
		typedef B<float> aliasB;

		aliasB b;
		const aliasA& ref = b.g();
	}

	//without template
	{
		std::cout << " test 4 " <<std::endl;
		S s;
		const S &rs1 = s;
	}
	{
		std::cout << " test 5 " <<std::endl;
		//EXPR WITH CLEANUP
		const S &rs2 = S();
	}
	{
		std::cout << " test 6 " <<std::endl;
		const C &rc(0);
		rc.m;
		const C &rc1 = C();
		rc1.m;
	}
	{
		std::cout << " test 7 " <<std::endl;
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
		std::cout << " test 8 " <<std::endl;
		C c;
		const C &rc1 = c;
		const C &rc3 = C(rc1);
		C &rs2 = c;
		const C &rc4 = cs_cs_fC(c);
		const C &rc5 = cs_s_fC(c);
		const C &rc6 = s_s_fC(c);
		const C &rc7 = s_cs_fC(c);
	}

	return 0;
}


