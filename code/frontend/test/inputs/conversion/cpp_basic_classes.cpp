
struct A {
	int i;
	float f();
};

float A::f() {
	return 1.0f;
}

int main() {
	; // this is required because of the clang compound source location bug
	#pragma test expect_ir(R"(
		def struct IMP_A {
			i : int<4>;
			ctor () {}
			lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
		};
		{ var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); }
	)")
	{ A a; }

	/*{
		A a;
		a.f();
	}*/
	return 0;
}
