class C {

public:
	C() : x(1) { }
	int x;

	int memb() { return this->x; }
	C* ptr() { return this; }
	C& ref() { return *this; }
	C  obj() { return *this; }
};

int main() {
	C c;

	int x = c.memb();
	C& rc = c.ref();
	C* pc = c.ptr();
	C nc = c.obj();
}
