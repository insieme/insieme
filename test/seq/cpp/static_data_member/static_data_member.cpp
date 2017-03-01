#include <iostream>

class Obj{

	static int private_value;
	static const int private_const_value;
	static const int private_const_value_special = 9; // TODO some special language feature see 9.2.4
	int member;

	public:

		static int public_value;
		static const int public_const_value;
		static const int public_const_value_special = 90; // TODO some special language feature see 9.2.4

		Obj(int v)
		: member(v){
		}

		int getPrivateVal()const {
			return private_value;
		}
		
		int getPrivateConstVal()const {
			return private_const_value;
		}
		
		int getPublicVal()const {
			return public_value;
		}

		int getPublicConstVal()const {
			return public_const_value;
		}
	
		int getPrivateConstSpecialVal()const {
			return private_const_value_special;
		}	
		int getPublicConstSpecialVal()const {
			return public_const_value_special;
		}
};

int Obj::private_value = 4;
int Obj::public_value = 40;
const int Obj::private_const_value = 5;
const int Obj::public_const_value = 50;

class SingletonClass {
		~SingletonClass(){}
		SingletonClass(){}
		static int rank;
		static int nprocs;
	public:
		static SingletonClass& instance();
		static void init();
		static int get_rank(){
                    return instance().instance().rank;
                }
		static int get_rank(int i){
                    return instance().instance().rank+i;
                }
		static int get_nprocs(){
                    return instance().nprocs;
                }
};

int SingletonClass::rank=-1;
int SingletonClass::nprocs=-1;

SingletonClass& SingletonClass::instance() {
	init();
	static SingletonClass* instance = new SingletonClass;
	return *instance;
}

void SingletonClass::init() {
	rank = 150;
	nprocs = 130;
}


int main(){

	Obj o(10);
	std::cout << "private " << o.getPrivateVal() << std::endl;
	std::cout << "public " << o.getPublicVal() << std::endl;
	std::cout << "private const" << o.getPrivateConstVal() << std::endl;
	std::cout << "public const" << o.getPublicConstVal() << std::endl;
	std::cout << "private const special" << o.getPrivateConstSpecialVal() << std::endl;
	std::cout << "public const special" << o.getPublicConstSpecialVal() << std::endl;


	//std::cout << Obj::private_value << std::endl;
	//std::cout << Obj::private_const_value << std::endl;
	std::cout << "public " << Obj::public_value << std::endl;
	std::cout << "public const" << Obj::public_const_value << std::endl;

        std::cout <<  "singletonTest rank: " << SingletonClass::get_rank() << std::endl;
        std::cout <<  "singletonTest nprocs: " << SingletonClass::get_nprocs() << std::endl;

        std::cout <<  "singletonTest rank: " << SingletonClass::instance().get_rank() << std::endl;
        std::cout <<  "singletonTest rank: " << SingletonClass::instance().get_rank(10) << std::endl;

        return 0;
}
