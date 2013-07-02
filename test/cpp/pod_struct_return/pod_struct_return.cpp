

struct A {};

struct B { int x; };

struct C { 
	int x; 
	C() : x(1) {} 
};

// empty POD
A createA() {
	return A();
}

// non-empty POD
B createB() {
	return B();
}

// not POD
/*
C createC() {
	return C();
}
*/

int main() {
	createA();
	createB();
//	createC();
	return 0;
}
