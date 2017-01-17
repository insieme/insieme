

#include <iostream>

int f(){
	return 15;
}

template<typename T>
class A{
	public:
	T a;
	A(T v) : a(v+1) {}
};

template<typename T>
class B : public A<T> {
	public:
	T b;
	B(T v) : A<T>(v), b(v) {}
};

int main (){

	B<int> obj(1);
	B<int>* bptr = &obj;

	if (A<int>* ptr  = dynamic_cast<A<int>*> (bptr)){
		std::cout << "dyn cast : " << ptr->a << std::endl;
	}

	if (A<int>* ptr  = static_cast<A<int>*> (bptr)){
		std::cout << "static cast : " << ptr->a << std::endl;
	}

	return 0;
}
