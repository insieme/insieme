#include <iostream>
#include <initializer_list>

//simple cases
struct T {
	T(std::initializer_list<int> l) {
		for(auto e : l) {
			std::cout << "T: " << e << std::endl;
		}
	}
};

void f(std::initializer_list<int> l) {
	for(auto e : l) {
		std::cout << "f: " << e << std::endl;
	}
}

int main (){

	//simple cases
	T({ 5 });
	T{42,43};
	f({ 5 });

	return 0;
}
