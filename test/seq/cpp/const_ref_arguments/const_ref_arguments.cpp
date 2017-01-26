
struct A {};

void f(A a, A b) {}
const A& g( const A& a) { return a; }


int main() {

	A a;

	// this case once lead to a missing de-ref operation
	f(g(a), A());
}
