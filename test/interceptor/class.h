namespace ns {

	// a class to be intersepted
	struct A {
		int a;
		int f(int x) { return x; }
	};
}
