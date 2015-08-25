
int main () {

	//===-------------------------------------------------------------------------------------------------------------------------------- NULL TO POINTER---===

	#pragma test expect_ir("type_cast(0, type(ptr<int<4>,f,f>))")
	(int*)0;

	#pragma test expect_ir("type_cast(0, type(ptr<real<4>,t,f>))")
	(const float*)0;

	#pragma test expect_ir("type_cast(0, type(ptr<real<4>,f,f>))")
	(float* const)0;

	#pragma test expect_ir("type_cast(0, type(ptr<int<4>,t,f>))")
	(const int* const)0;

	//===-------------------------------------------------------------------------------------------------------------------------------- POINTER TO BOOL---===

	// TODO: move this somewhere else? Because there is no actual clang::CK_PointerToBoolean involved here
	#pragma test expect_ir("{ decl ref<ptr<int<4>,f,f>,f,f> v0 = var(type_cast(0, type(ptr<int<4>,f,f>))); if(ptr_eq((*v0), ptr_null)) { } }")
	{
		int* p = 0;
		if(p) { }
	}

}
