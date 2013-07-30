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


template <class T>
T f(T value){
	static T a =0;
	std::cout << "call: " << a << std::endl;
	return a;
}

int main(){

	{
	std::cout << Obj<int>(4).getVal() << std::endl;
	std::cout << Obj<float>(4.0f).getVal() << std::endl;
	std::cout << Obj<int>(3).getVal() << std::endl;
	std::cout << Obj<float>(3.0f).getVal() << std::endl;
	}

	{
	std::cout << f(3u) << std::endl;
	std::cout << f(3000) << std::endl;
	std::cout << f(3.0f) << std::endl;
	std::cout << f(3.0) << std::endl;
	}
	return 0;
}
