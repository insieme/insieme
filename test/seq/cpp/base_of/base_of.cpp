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
		std::cout << "Base is Base of Derived" <<std::endl;
	if (__is_base_of(Base, Derived) == false)
		std::cout << "Base is not Base of Derived" <<std::endl;
	if (__is_base_of(Base, Unrelated) == true)
		std::cout << "Base is Base of Unrelated" <<std::endl;
	if (__is_base_of(Base, Unrelated) == false)
		std::cout << "Base is not Base of Unrelated" <<std::endl;
}
