#include <iostream>

struct POD {
	int a;
};

struct POD1 {
	int a;
};

int main() {
	POD s = {1};
	std::cout << s.a << std::endl;
	
	POD *f = new POD;
	std::cout << f->a << std::endl;

	POD1 *f1 = new POD1();
	std::cout << f1->a << std::endl;
	
	std::cout << (POD){10}.a << std::endl;

	return 0;
}
