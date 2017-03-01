#include <iostream>

template <class T>
T initFunc() {
	return T();
}

template<>
int initFunc(){
	return 15;
}

template <class T>
struct Owner {
	static T const e;
};

template <class T>
T const Owner<T>::e = initFunc<T>();


int main (){

	int a 	= Owner<int>::e;
	float b = Owner<float>::e;

	std::cout << "int: "   << a << std::endl;
	std::cout << "float: " << b << std::endl;

	return 0;
}
