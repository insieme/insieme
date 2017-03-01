struct A {
	int m;
	A() {};
};

struct DA : public A {
	DA() : A() {};
};


struct B { 
	A* pa;
	B(A* pa) : pa(pa) {};
};

struct C : public B {
	C(DA* pa) : B(pa) {};
};

int main() {
	DA* pa = 0;
	C c(pa);
}
