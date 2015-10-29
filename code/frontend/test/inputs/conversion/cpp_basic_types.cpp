
int producer() { return 5; }
void consumer(int&& i) { }


int main() {
	
	#pragma test expect_ir("{ decl ref<int<4>> v0; decl ref<int<4>,f,f,cpp_ref> v1 = ref_cast(v0, type_lit(f), type_lit(f), type_lit(cpp_ref)); }")
	{
		int i;
		int &ref_i = i;
	}
	
	#pragma test expect_ir("{ decl ref<int<4>> v0; decl ref<int<4>,t,f,cpp_ref> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)); }")
	{
		int i;
		const int& ref_i = i;
	}
	
	//pragma test expect_ir(R"(function (ref<int<4>,f,f,cpp_rref> v1) -> unit { }(function () -> int<4> { return 5; }()))")
	//consumer(producer());

	return 0;
}
