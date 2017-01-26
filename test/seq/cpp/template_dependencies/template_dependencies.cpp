
#include <set>

using namespace std;

struct A {
	int x;
};

struct B {
	set<A> as;
};


int main() {

	B b;
	return 0;
}
