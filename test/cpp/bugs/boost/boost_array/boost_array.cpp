#include <iostream>
#include <boost/array.hpp>

int main() {
	boost::array<int, 3> a;
	a[0] = 10;
	std::cout << a[0];
}
