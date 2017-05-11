
#include <vector>

template<typename T>
struct V {
	std::vector<T> mem;
	V(int a) : mem(a) {}
};

struct B {
	B(int a) {}
};

struct A {
	B mem;
	A(int a) : mem(a) {}
};

int main() {
	V<int> v(1);
	A a(2);
}
