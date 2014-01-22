class A {
	int * p;
public:
	A(int n) : p(new int()) {}
};

int main() {
	A a(3);
	return 0;
}
