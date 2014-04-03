#include <iostream>
#include <vector>

using namespace std;

template<typename D>
struct Base {

	int x;

	// use derived in a field
	D* d;
	vector<D*> v;
	vector<void(D::*)()> mf;
	vector< vector<void(D::*)()> > mmf;

	Base(int x = 0) : x(x) {}

	// use derived in a call
	void f() {
		static_cast<D*>(this)->g();
	}

	void doSomething(int a, int b);

};

template<typename D>
void Base<D>::doSomething(int a, int b) {};

struct Derived : public Base<Derived> {

	Derived(int x = 0) : Base<Derived>(x) {} 

	void g() {
		cout << "g";
	}

};

int main() {
	Derived d(2);

	d.doSomething(1,2);
	d.f();
	d.g();
	cout << endl;
}


