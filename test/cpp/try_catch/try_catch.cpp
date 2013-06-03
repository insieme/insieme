#include <iostream>

int main() {

	try {
		throw 10;
	} catch(int e) {
		std::cout << "exception caught";
	}

	return 0;
}
