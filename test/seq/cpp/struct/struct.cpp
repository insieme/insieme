#include <iostream>

struct A {
	A() {};
	int x;
	int y;
};

//return a reference to an element of A
const A& getConstRefA() {
	A* x = new A();
	x->x = 1;
	return *x;
}

//return a reference to an element of A
A& getRefA() {
	A * x = new A();
	x->x = 1;
	return *x;
}

//return an element of A
A getA() {
	A * x = new A();
	x->x = 1;
	return *x;
}


//test functions
void testRef(A * a) {
	//ref
	A& rng = a ? *a : getRefA();
	std::cout << "ref " << rng.x << "\n";
}
void testConstRef(const A * a) {
	//const ref
	const A& rng = a ? *a : getConstRefA();
	std::cout << "const ref " << rng.x << "\n";
}
void testValue(A * a) {
	//value
	A rng = a ? *a : getA();
	std::cout << "val " << rng.x << "\n";
}




int main() {
{
	//test with ptr false
	testRef(NULL);
	testConstRef(NULL);
	testValue(NULL);
}

	A* x = new A();
	{
		//test with ptr true
		x->x=2;
		testRef(x);
		testConstRef(x);
		testValue(x);
	}
	delete x;
	return 0;
}
