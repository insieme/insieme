#include <iostream>

class C {
	int m_a;

	public:
		bool operator==(const C& o) const { return (m_a == o.m_a); }
};

int main() {
	C c1;
	C c2;
	std::cout << "true == " << c1 == c2;
	return 0;
}
