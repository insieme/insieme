

#include <iostream>

struct B{
	int a;
	int b;
};


int main (){
	int size = 7;
	B** ptr = new B*[size];
	std::cout << "size: " << sizeof(ptr) << std::endl;
	delete[] ptr;
	return 0;
}
