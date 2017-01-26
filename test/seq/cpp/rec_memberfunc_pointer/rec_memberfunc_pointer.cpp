#include <iostream>
#include <vector>

using namespace std;

struct A {

	int a;
	A* next;

	vector<void (A::*)()> funs;

	A(int a = 0) : a(a), next(0) {}	

	void f() { cout << "f"; };
	void g() { cout << "g"; };

};

int main() {

	A a;

	a.funs.push_back(&A::f);
	a.funs.push_back(&A::g);

	(a.*(a.funs[0]))();
	(a.*(a.funs[1]))();
}
