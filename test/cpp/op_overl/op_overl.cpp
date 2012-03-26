#include <stdio.h>

class C {
	int m_a;

public:
	bool operator==(const C& o) const {
		printf("const C::operator==\n");
		return (m_a == o.m_a);
	}
	bool operator==(const C& o) {
		printf("C::operator==\n");
		this->m_a++;
		return (m_a == o.m_a);
	}

	friend bool operator!=(const C& t, const C& o);
	friend bool operator<=(const C& t, const C* o);

	friend int operator+(const C& t);

	int operator()(int x, int y, int z) const {
		printf("const C::operator()\n");
		return (x + y + z + this->m_a);
	}
	int operator()(int x, int y, int z) {
		printf("C::operator()\n");
		return (x + y + z + this->m_a);
	}
};

bool operator!=(const C& t, const C& o) {
	printf("C::operator!=\n");
	return (t.m_a != o.m_a);
}
bool operator<=(const C& t, const C* o) {
	printf("C::operator<=\n");
	return (t.m_a <= o->m_a);
}

int operator+(const C& t) {
	printf("C::operator+\n");
	return t.m_a;
}

int main() {
	C c1;
	C c2;
	const C* pc = &c1;
	C& rc = c1;

	+c1;
	c1 == c2;
	c1 != c2;
	c1 <= &c2;
	c1(1, 2, 3);

	+(*pc);
	*pc == c2;
	*pc != c2;
	*pc <= &c2;
	(*pc)(1, 2, 3);

	c2 == *pc;
	c2 != *pc;
	c2 <= pc;
	c2(1, 2, 3);

	+rc;
	rc == c2;
	rc != c2;
	rc <= &c2;
	rc(1, 2, 3);

	c2 == rc;
	c2 != rc;
	c2 <= &rc;

	return 0;
}
