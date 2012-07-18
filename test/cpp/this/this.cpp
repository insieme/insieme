#include <stdio.h>

class C {
public:
	C() : x(1) { }
	C(const C& o) { }
	int x;

	int memb() { return this->x; }
	C* ptr() { return this; }
	C& ref() { return *this; }
	C  obj() { return *this; }
};

int main() {
	C c;

	int x = c.memb();
	printf("1 == %d", c.memb());

	C& rc = c.ref();
	printf("1 == %d", rc.x );

	C* pc = c.ptr();
	printf("1 == %d", pc->x );

	C nc = c.obj();
	printf("1 == %d", nc.x );
}
