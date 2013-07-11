
#include <iostream>


int count() {
	static int c = 0;
	return ++c;
}

int inc(int x = 1) {
	static int c = 0;
	c += x;
	return c;
}


int main() {

	std::cout << count() << "\n";
	std::cout << count() << "\n";
	std::cout << count() << "\n";
	std::cout << count() << "\n";
	std::cout << count() << "\n";

	std::cout << "\n";

	std::cout << inc() << "\n";
	std::cout << inc(2) << "\n";
	std::cout << inc(3) << "\n";
	std::cout << inc(2) << "\n";
	std::cout << inc() << "\n";


	return 0;
}
