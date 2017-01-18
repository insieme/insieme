

struct A {
	double x; float y;
	A(int x) : x(x), y(x) {};
	const double& getX() const { return x; };
	const float& getY() const { return y; };
};


int main() {

	A a(12);
	const A& b = a;

	if (b.getX() > 10) {
		return 0;
	}

	return 1;
}
	

