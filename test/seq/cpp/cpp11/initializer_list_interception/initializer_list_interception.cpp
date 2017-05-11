
#include <vector>
#include <initializer_list>
#include "header.h"

template<typename T>
struct A {
	std::vector<T> mem;
	A(int a, int b) : mem({a, b}) {}
	A(std::initializer_list<T> l) {}
	A(const std::initializer_list<T>& l, int i) {}
};

int main() {
	A<int> a(1, 2);
	A<double> b({3.0, 4.0});

	std::initializer_list<int> list{5, 6};
	A<int>(list, 7);

	Intercepted({8, 9});
}
