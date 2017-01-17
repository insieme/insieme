
struct B;

struct A {
	B f();	
};

struct B : public A {

};


int main() {

	A a;
	return 0;
}

B A::f() {
	return B();
}
