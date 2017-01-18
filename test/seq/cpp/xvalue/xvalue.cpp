#include <utility>

struct Base { };

struct Derived;
Derived&& move(Derived& d) { return static_cast<Derived&&>(d); }

struct Derived : public Base {
	Derived() {}
	Derived(Derived&& other) : Base(move(other)) {}
};


struct Derived2 : public Base {
	Derived2() {}
	Derived2(Derived2&& other) : Base(std::move(other)) {}
};

int main() {

	Derived d1;
	Derived d2(move(d1));

	Derived2 d3;
	Derived2 d4(std::move(d3));

	return 0;
}
