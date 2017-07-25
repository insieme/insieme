
struct T {
	T(int i = 0) {}
};

struct S {
	T t;
};

int main() {
	// creating an instance of S will implicitly call the default constructor of T.
	// if we didn't include a no-argument default constructor in the IR, this will fail
	S s;
}
