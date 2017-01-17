#include <iostream>

class A {
    public:
	template<typename R, typename T>
	R getSizeOf() const;

	template<typename R, typename T>
	R getSizeOf() ;
    
        int mF() { return 0; }
};

class B {
    public:
    	B(int x=0) { std::cout << "B()" << std::endl; }

        template<typename R, typename T>
        R getSizeOf() {
		std::cout << "B::getSizeOf" << std::endl;
		std::cout << "Size of given type: " << sizeof(T) << std::endl;
		std::cout << "Size of given ret type: " << sizeof(R) << std::endl;
		return R();
        }

	template<typename R, typename T>
        R getSizeOf() const{
		std::cout << "B::getSizeOf const" << std::endl;
		std::cout << "CONST : Size of given type: " << sizeof(T) << std::endl;
		std::cout << "CONST : Size of given ret type: " << sizeof(R) << std::endl;
		return R();
        }
};

template<typename R, typename T>
R A::getSizeOf() {
	std::cout << "A::getSizeOf" << std::endl;
	std::cout << "Size of given type: " << sizeof(T) << std::endl;
	std::cout << "Size of given ret type: " << sizeof(R) << std::endl;
	return R();
}

template<typename R, typename T>
R A::getSizeOf() const{
	std::cout << "A::getSizeOf const" << std::endl;
	std::cout << "CONST : Size of given type: " << sizeof(T) << std::endl;
	std::cout << "CONST : Size of given ret type: " << sizeof(R) << std::endl;
	return R();
}

template<typename T>
T getSizeOf() {
	std::cout << "Size of given type: " << sizeof(T) << std::endl;
	return T();
}

int main() {
	{
		A a;
		int i = a.getSizeOf<int, int>();
		double d = a.getSizeOf<double, double>();
		A b = a.getSizeOf<A, A>();
	
		const A& ra = a;
		int i1 = ra.getSizeOf<int, int>();
	}
	{
		B b;
		int i = b.getSizeOf<int, int>();
		double d = b.getSizeOf<double, double>();
		B b1 = b.getSizeOf<B, B>();
	}
	{
		const B& b = B();
		int i = b.getSizeOf<int, int>();
		double d = b.getSizeOf<double, double>();
		const B b1 = b.getSizeOf<B, B>();
	}
	{
		int i = getSizeOf<int>();
		double d = getSizeOf<double>();
	}
	return 0;
}
