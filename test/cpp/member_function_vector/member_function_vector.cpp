
#include <vector>

using namespace std;

template<typename D>
struct F {

	typedef void(D::*mFun)();
	
	vector<mFun> ops;

	F() {
		&D::f;
	}
};


struct A : public F<A> {

	void f() {};
	void doSomething(int a) { };

};

int main() {

	A a;	

	a.doSomething(12);


	return 0;
}
