#include <stdio.h>

struct A {
	virtual void doStuff() = 0;
};

struct B : public A {
	virtual void doStuff() {
		#pragma omp parallel num_threads(2)
		printf("Bla\n");
	}
};

int main() {
	B b;
	b.doStuff();
	
	A* a = &b;
	a->doStuff();
	
	return 0;
}