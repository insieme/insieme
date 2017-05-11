#include <iostream>

struct POD {
	int a;
};

int main() {
	POD s = {1};
	std::cout << s.a << std::endl;

	POD *f = new POD();
	std::cout << f->a << std::endl;

	std::cout << (POD){10}.a << std::endl;

	return 0;
}
