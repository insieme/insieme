
template <typename E>
struct T {
	T(int i = 0) {}
};

template <typename E>
struct S {
	T<E> t;
};

int main() {
	// creating an instance of S will implicitly call the default constructor of T.
	// if we didn't include a no-argument default constructor in the IR, this will fail

	// Note that this test here reveals a different behavior than the test "ctor_default_argument". here we have templated classes, which will
	// cause the default argument of class T to be uninstantiated.
	// This test will ensure that we correctly instantiate the default argument in this case.
	S<int> s;
}
