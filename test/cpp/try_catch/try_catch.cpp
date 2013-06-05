#include <iostream>

int main() {

	try {
		throw 10;
		int x = 10;
		x < 10 ? throw 10 : 1;
		x < 10 ? 1 : throw 10;
	} catch(int e) {
		std::cout << "exception caught " << e;
	}

	return 0;
}
