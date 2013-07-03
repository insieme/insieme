#include <iostream>

struct POD {
	int a;
};

/*
void* operator new(size_t size, int i) {
	std::cout << "operator new(size_t, int) " << i << std::endl;
	return ::operator new(size);
}
*/
int main() {
	POD *f; 
	{
	f = new POD;
	std::cout << f->a << std::endl;
	delete f;
	}

	/*
	{
	f = new (1) POD;
	std::cout << f->a << std::endl;
 	delete f;
	}
 	*/

	return 0;
}
