#include <iostream>
#include "impl.h"
struct K {
	K() { std::cout << "K()" << std::endl; }

};
int s_d1(const S& s, const K& k) {
	return 1;
}
int s_d(const S& s, const K& k) {
	return s_d1(s,k);
}

int s_d(const S& s) {
	return s_d(s,K());
}

int main() {

	S s1;
	S s2;
	
	{
		const S &irs1 = s1;
		const S &irs2 = s2;

		const S& rs1=irs1, rs2=irs2;
		std::cout << irs1.m << std::endl;
		std::cout << irs2.m << std::endl;
		std::cout << rs1.m << std::endl;
		std::cout << rs2.m << std::endl;
	}
	{
		const S& rs1=s1, rs2=s2;

		const S cls1=s1;
		S ls1=s1;

		std::cout << cls1.m << std::endl;
		std::cout << ls1.m << std::endl;
		std::cout << rs1.m << std::endl;
		std::cout << rs2.m << std::endl;
	}
	{
		const S &rs = s1;
		const S &rs2 = rs.f();

		const S &rs3 = s1.f();
	}
	{
		const S &rs = s1;
		const S &rs2 = rs();

		const S &rs3 = s1();
		const S& v = S();
	}
	/*
	{	
		const S& rs1=s1;
		const S& rs2=s2;
	}
	*/
	/*	
	S s;
	K k;
	s_d(s,k);
	s_d(s);
*/
}
