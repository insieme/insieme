
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

	return 0;
}
