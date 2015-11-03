
class A {
	int i;
	float f();
};

int main() {
	#pragma test expect_ir("decl ref<int<4>> v0; decl ref<int<4>,t,f,cpp_ref> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref));")
	A a;
	return 0;
}
