#include <stdio.h>

//nested class
class Outer {
	class Inner {
	public:
		int inner;
		Inner() : inner(10) {}
	};
	Inner i;
public:
	Outer() : outer(5) {}
	int outer;
	int get_inner_val() { return i.inner; }
};

int main() {
	//nested class
	Outer o;

	printf("Outer::get_inner_val() %d\n", o.get_inner_val());
	printf("Outer::outer %d\n", o.outer);
	return 0;
}
