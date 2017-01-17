#include "impl.h"
S g() {
	return S();
}

S S::f() const {
	return g();
}


