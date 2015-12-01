
struct A {
	int i;
	float f();
};

float A::f() {
	return 1.0f;
}

#define A_IR R"(
def struct IMP_A {
	i : int<4>;
	lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

struct B {
	int i;
	float f() { return 1.0f; }
};

#define B_IR R"(
def struct IMP_B {
	i : int<4>;
	lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

struct C {
	unsigned x;
	C() {
		x = 0u;
	}
	C(unsigned y) {
		x = y;
	}
};

#define C_IR R"(
def struct IMP_C {
	x : uint<4>;
	ctor() { cxx_style_assignment(x, 0u); }
	ctor(y : uint<4>) { cxx_style_assignment(x, y); }
};)"

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(A_IR,R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); } )")
	{ A a; }
	
	#pragma test expect_ir(A_IR,R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); a.IMP_f(); } )")
	{
		A a;
		a.f();
	}
	
	#pragma test expect_ir(B_IR,R"( { var ref<IMP_B> b = IMP_B::(ref_var(type_lit(IMP_B))); b.IMP_f(); } )")
	{ 
		B b;
		b.f();
	}
	
	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c1 = IMP_C::(ref_var(type_lit(IMP_C))); } )")
	{
		C c1;
	}
	
	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c = IMP_C::(ref_var(type_lit(IMP_C)), 6u); } )")
	{
		C c2(6u);
	}

	return 0;
}
