#include <iostream>



template <class T>
class TObj{

	static const int value = 4;
	T member;

	public:

		TObj(T v)
		: member(v){
		}

		int getVal()const {
			return value;
		}
};

template <class T>
class TObj1{

	static int value;
	T member;

	public:

		TObj1(T v)
		: member(v){
		}

		int getVal()const {
			return value;
		}
};

template<class T>
int TObj1<T>::value = 4;


template <class T>
class TObj2{

	static const int value;
	T member;

	public:

		TObj2(T v)
		: member(v){
		}

		int getVal()const {
			return value;
		}
};


template<class T>
const int TObj2<T>::value = 4;

class Obj{

	static int value;
	int member;

	public:

		Obj(int v)
		: member(v){
		}

		int getVal()const {
			return value;
		}
};

int Obj::value = 4;

int main(){

	{
		std::cout << Obj(4).getVal() << std::endl;
		std::cout << Obj(3).getVal() << std::endl;
	}
	{
		std::cout << TObj<int>(4).getVal() << std::endl;
		std::cout << TObj<float>(4.0f).getVal() << std::endl;
		std::cout << TObj<int>(3).getVal() << std::endl;
		std::cout << TObj<float>(3.0f).getVal() << std::endl;
	}

	{
		std::cout << TObj1<int>(4).getVal() << std::endl;
		std::cout << TObj1<float>(4.0f).getVal() << std::endl;
		std::cout << TObj1<int>(3).getVal() << std::endl;
		std::cout << TObj1<float>(3.0f).getVal() << std::endl;
	}
	{
		std::cout << TObj2<int>(4).getVal() << std::endl;
		std::cout << TObj2<float>(4.0f).getVal() << std::endl;
		std::cout << TObj2<int>(3).getVal() << std::endl;
		std::cout << TObj2<float>(3.0f).getVal() << std::endl;
	}

	return 0;
}
