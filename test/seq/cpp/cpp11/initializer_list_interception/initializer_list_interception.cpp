
#include <vector>

template<typename T>
struct A {
	std::vector<T> mem;
	A(int a, int b) : mem({a, b}) {}
};

int main() {
	A<int> a(1, 2);
}
