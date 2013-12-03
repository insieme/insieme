#include <iostream>

struct Base{
	int a;
};

struct Unrelated{
	int x;
};

struct Derived : public Base{

	float b;
	Derived() : Base() {}

};

int main (){

	if (__is_base_of(Base, Derived) == true)
		std::cout << "is Base of " <<std::endl;
	if (__is_base_of(Base, Unrelated) == false)
		std::cout << "is NOT Base of " <<std::endl;
}
