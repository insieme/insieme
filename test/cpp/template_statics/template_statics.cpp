#include <iostream>



template <class T>
class Obj{

	static const int value = 4;
	T member;

	public:

		Obj(T v)
		: member(v){
		}

		int getVal()const {
			return value;
		}
};


int main(){


	std::cout << Obj<int>(4).getVal() << std::endl;
	std::cout << Obj<float>(4.0f).getVal() << std::endl;
	std::cout << Obj<int>(3).getVal() << std::endl;
	std::cout << Obj<float>(3.0f).getVal() << std::endl;

	return 0;
}
