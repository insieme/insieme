
#include <iostream>


int g(int x) {
	std::cout << "Running ..\n";
	return x + 10;
}


int f(int x) {
	static const int y = g(x);
	return y;
}


int h(int x){
	static int y = g(x);
	return y;
}

int i(int x){
	static float y;
	y++;
	return x;
}



int main() {


	std::cout << f(10) << "\n";
	std::cout << f(20) << "\n";

	std::cout << h(10) << "\n";
	std::cout << h(20) << "\n";

	std::cout << i(10) << "\n";
	std::cout << i(20) << "\n";
	return 0;
}
