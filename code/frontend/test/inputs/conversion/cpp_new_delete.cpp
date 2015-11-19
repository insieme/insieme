
int main() {
	;
	
	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new(type_lit(int<4>))));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int *i = new int;
		delete i;
	}
	
	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new_init(42)));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int *i = new int { 42 };
		delete i;
	}
	
	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new(type_lit(int<4>))));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int *arri = new int[50];
		delete [] arri;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new(type_lit(int<4>))));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int *arri = new int[50] {1,2,3};
		delete [] arri;
	}

	return 0;
}
