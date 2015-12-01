
struct A {
	int i;
	float f();
};

float A::f() {
	return 1.0f;
}

#define A_IR                                                                                                                                                   \
	R"(
def struct IMP_A {
	i : int<4>;
	lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

int main() {
	; // this is required because of the clang compound source location bug
	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); } )")
	{ A a; }

	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); a.IMP_f(); } )")
	{
		A a;
		a.f();
	}

	{
		A* a = new A();
		delete a;
	}
	return 0;
}
