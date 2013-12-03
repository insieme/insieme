#include <array>
#include <iostream>

int main() {
	//is CXX11
	std::array<int, 3> a;
	a.fill(100);
	std::cout << a[0];
}
