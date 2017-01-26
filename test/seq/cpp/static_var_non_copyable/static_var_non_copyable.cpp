
#include "intercepted_stuff.h"


void f() {
	static inner::M m;
}

int main() {
	f();
	f();
	return 0;
}
