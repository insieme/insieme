#include <iostream>

struct Crystal {
	static void meth() {
		std::cout << "Crystal::meth" << std::endl;
	}		
};

int main() {
	Crystal::meth();
	return 0;
}
