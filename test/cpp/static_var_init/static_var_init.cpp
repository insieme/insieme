
#include <iostream>


int g(int x) {
	std::cout << "Running ..\n";
	return x + 10;
}


int f(int x) {
	static const int y = g(x);
	return y;
}



int main() {

	std::cout << f(10) << "\n";
	std::cout << f(20) << "\n";

	return 0;
}
