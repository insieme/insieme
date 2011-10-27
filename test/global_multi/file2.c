
extern var1;

int func2() {
	static int var1 = 0;
	return ++var1;
}

static int __func3() {
	static int var2 = 10;
	return var2/var1;
}

int func3() { return __func3(); }
