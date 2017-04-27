
#include <vector>
#include <initializer_list>

template<typename T>
struct A {
	std::vector<T> mem;
	A(int a, int b) : mem({a, b}) {}
	A(std::initializer_list<T> l) {}
};

int main() {
	A<int> a(1, 2);
	A<double> b({3.0, 4.0});
}
