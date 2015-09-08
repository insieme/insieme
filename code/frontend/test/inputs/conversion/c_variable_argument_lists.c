
int bar() { return 0; }

void bla(int a, int b, ...) {
}

void foo(char a, ...) {
}


int main() {

	#pragma test expect_ir("{ using \"ext.varargs\"; lambda (int<4> v1, int<4> v2, var_list v3) -> unit { }(1, 2, varlist_pack(tuple())); }")
	{ bla(1,2); }
	
	//#pragma test expect_ir("fun(ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> unit { }(2, 3, varlist_pack((ptr_from_array(Risiko))))")
	//bla(2,3, "Risiko");
	//
	//#pragma test expect_ir("fun(ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> unit { }(5, 6, varlist_pack((5.0E+0, 17)))")
	//bla(5,6, 5.0, 17);
	//
	//#pragma test expect_ir("fun(ref<char,f,f> v1) -> unit { }(num_cast('a', type<char>), varlist_pack(()))")
	//foo('a');
	//
	//#pragma test expect_ir("fun(ref<char,f,f> v1) -> unit { }(num_cast('a', type<char>), varlist_pack((fun() -> int<4> {\\n    return 0;\\n}())))")
	//foo('a', bar());

	return 0;
}


