
struct SimplestConstructor {
	SimplestConstructor() = default;
	~SimplestConstructor() = default;
};

#define SimplestConstructor_IR R"( def struct IMP_SimplestConstructor { }; )"

int main() {
	;

	// Base types ----------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new(type_lit(int<4>))));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int;
		delete i;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_ref(ref_new_init(42)));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int{42};
		delete i;
	}

	// Base type arrays ----------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ref_var_init(ptr_from_array(ref_new(type_lit(array<int<4>,50>))));
		ref_delete(ptr_to_array(*i));
	})")
	{
		int* arri = new int[50];
		delete[] arri;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i =  ref_var_init(ptr_from_array(ref_new_init(array_create(type_lit(50), [1,2,3]))));
		ref_delete(ptr_to_array(*i));
	})")
	{
		int* arri = new int[50]{1, 2, 3};
		delete[] arri;
	}

	// Class types ---------------------------------------------------------------------------------------------------------------------------------------------
	
	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ref_var_init(ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor)))));
		ref_delete(ptr_to_ref(*v0));
	})")
	{
		SimplestConstructor *simple = new SimplestConstructor;
		delete simple;
	}

	//# pragma test expect_ir(SimplestConstructor_IR, R"({
	//	var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ref_var_init(ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor)))));
	//	ref_delete(ptr_to_ref(*v0));
	//})")
	//{
	//	SimplestConstructor* arrsimple = new SimplestConstructor[3];
	//	delete [] arrsimple;
	//}


	return 0;
}
