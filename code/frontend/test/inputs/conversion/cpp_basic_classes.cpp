
struct A {
	int i;
	float f();
};

float A::f() {
	return 1.0f;
}

int main() {
	; // this is required because of the clang compound source location bug
	/*pragma test expect_ir(R"(
		def struct IMP_A {
			i : int<4>;
			ctor() {}
			ctor(other : ref<IMP_A,t,f,cpp_ref>) {}
			ctor(other : ref<IMP_A,f,f,cpp_rref>) {}
			dtor() {}
			lambda IMP_f : () -> real<4> {}
		};
		{ var ref<IMP_A> a; }
	)")*/
	//{ A a; }

	/*{
		A a;
		a.f();
	}*/
	return 0;
}
