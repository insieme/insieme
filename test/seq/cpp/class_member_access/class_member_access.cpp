struct C {
	int a;
	C() : a(0) {}
};

int main() {
	C c;

	// test both l-value and r-value access
	return c.a + C().a;
}