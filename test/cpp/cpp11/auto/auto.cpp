#include <iostream>

int f(){
	return 15;
}

int main (){
	auto i = f();
	std::cout << i << std::endl;
	return 0;
}
