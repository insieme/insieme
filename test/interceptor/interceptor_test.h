namespace ns {

	// a function to be intersected
	int func(int i) {
		int x = i;
		return x;
	}

	// a class to be intersected
	struct A {
		int f(int x) { return x; }
	};
}
