
int producer() {
	return 5;
}
void consumer(int&& i) {}

using philipp = int;
using driver = philipp;

int main() {
#pragma test expect_ir("{ var ref<bool> v0; cxx_style_assignment(v0, true); cxx_style_assignment(v0, false); }")
	{
		bool a;
		a = true;
		a = false;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,f,f,cpp_ref> v1 = ref_cast(v0, type_lit(f), type_lit(f), type_lit(cpp_ref)); }")
	{
		int i;
		int& ref_i = i;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,t,f,cpp_ref> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)); }")
	{
		int i;
		const int& ref_i = i;
	}

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var_init(0);")
	auto var0 = 0;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var_init(1);")
	decltype(var0) var1 = 1;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var_init(2);")
	philipp var2 = 2;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var_init(3);")
	driver var3 = 3;

	#pragma test expect_ir(R"(
		def IMP_consumer = function (v1 : ref<int<4>,f,f,cpp_rref>) -> unit { };
		def IMP_producer = () -> int<4> { return 5; };
		IMP_consumer(IMP_producer())
	)")
	consumer(producer());


	return 0;
}
