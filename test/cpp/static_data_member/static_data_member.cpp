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
}
