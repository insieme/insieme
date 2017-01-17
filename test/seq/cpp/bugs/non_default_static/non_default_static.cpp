
#include <iostream>


struct Obj{
	int value;
	Obj(int v):value(v){}
};

int first(int i) {
	static const Obj o(i);
	return o.value;
}

int main (){

	std::cout << first(12) << "\n";
	std::cout << first(14) << "\n";
	std::cout << first(16) << "\n";

	return 0;
}

