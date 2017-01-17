#include <iostream>
template <typename A, typename B>
class Obj{

	typedef Obj<A,B> self_type;
	typedef int (self_type::*func_ptr)();
	typedef int (Obj<A,B>::*func_ptr2)();

	int f0(){
		return 0;
	}
	int f1(){
		return 1;
	}

	typedef int (self_type::*func_ptr3)(bool);
	typedef int (Obj<A,B>::*func_ptr4)(bool);

	int g0(bool b){
		if (b) return 1;
		else return 0;
	}
	int g1(bool b){
		if (b) return 0;
		else return 1;
	}

public:
	void problem0(){
		static func_ptr const array[2] = {
			&self_type::f0,
			&self_type::f1
		};
		func_ptr ptr = array[0];
		std::cout << (this->*ptr)() << std::endl;
		ptr = array[1];
		std::cout << (this->*ptr)() << std::endl;
	}
	void problem1(){
		static func_ptr const array[2] = {
			&Obj<A,B>::f0,
			&Obj<A,B>::f1
		};
		func_ptr ptr = array[0];
		std::cout << (this->*ptr)() << std::endl;
		ptr = array[1];
		std::cout << (this->*ptr)() << std::endl;
	}
	void problem2(){
		static func_ptr2 const array[2] = {
			&Obj<A,B>::f0,
			&Obj<A,B>::f1
		};
		func_ptr2 ptr = array[0];
		std::cout << (this->*ptr)() << std::endl;
		ptr = array[1];
		std::cout << (this->*ptr)() << std::endl;
	}

	//////////////////////////////////////////////////

	void problem3(bool a){
		static func_ptr3 const array[2] = {
			&Obj<A,B>::g0,
			&Obj<A,B>::g1,
		};
		func_ptr3 ptr = array[0];
		std::cout << (this->*ptr)(a) << std::endl;
		ptr = array[1];
		std::cout << (this->*ptr)(a) << std::endl;
	}
};


int main(){	

	// no instanciation, no fun!
	Obj<int,float> o;

	o.problem0();
	o.problem1();
	o.problem2();
	o.problem3(true);

	return 0;
}
