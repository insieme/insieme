
class A {
	int i;
	float f();
};

int main() {
	#pragma test expect_ir("42;")
	A a;
	return 0;
}
