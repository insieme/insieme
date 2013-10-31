

#include <iostream>

class Super{

	static int a;

	public:
		static int f(){
			std::cout << "a: " << a << std::endl;
			a++;
			return 0;
		}
};

int  Super::a = 0;

int main (){
	return Super::f();
	return Super::f();
	return Super::f();
}
