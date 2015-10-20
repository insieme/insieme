
int foo(int a) { return a; }

int main() {	

	// TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0;")
	int(*ifFuncPtr)(float);

	#pragma test expect_ir("var ref<ptr<()->unit,t,f>,f,f> v0;")
	void(*vvFuncPtr)(void);

	// EXPRESSIONS //////////////////////////////////////////////////////////////
	
	#define FOO_FUN "alias foo_type = (int<4>) -> int<4>; def foo = (a: int<4>)->int<4> { return a; }; alias foo_ptr_type = ptr<foo_type,t,f>; "
	#define C_STYLE_ASSIGN "def c_ass = (v1: ref<'a,f,'b>, v2: 'a) -> 'a { v1 = v2; return *v1; };"
	{ }

	#pragma test expect_ir(FOO_FUN "{ ptr_of_function(foo); 1; }")
	{
		&foo;
		1; // to make INSPIRE different from next case
	}
	
	#pragma test expect_ir(FOO_FUN "{ ptr_of_function(foo); 2; }")
	{
		foo;
		2; // to make INSPIRE different from previous case
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; !ptr_ne(*v0, ptr_null(type_lit(foo_type), type_lit(t), type_lit(f))); }")
	{
		int (*ptr)(int);
		!ptr;
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; ptr_deref(*v0)(5); }")
	{
		int (*ptr)(int);
		ptr(5);
	}
	
	#pragma test expect_ir(FOO_FUN C_STYLE_ASSIGN "{ var ref<foo_ptr_type,f,f> v0; c_ass(v0, ptr_of_function(foo)); }")
	{
		int (*ptr)(int);
		ptr = foo;
	}
	
	#pragma test expect_ir(FOO_FUN C_STYLE_ASSIGN "{ var ref<foo_ptr_type,f,f> v0; c_ass(v0, ptr_of_function(foo)); }")
	{
		int (*ptr)(int);
		ptr = &foo;
	}

	#pragma test expect_ir(FOO_FUN "{ ptr_deref(ptr_of_function(foo))(5); }")
	{
		(*(&foo))(5);
	}

	// INIT EXPRESSIONS ///////////////////////////////////////////////////////////
	
	#pragma test expect_ir(FOO_FUN C_STYLE_ASSIGN "{ var ref<foo_ptr_type,f,f> v0 = ref_var(ptr_of_function(foo)); }")
	{
		int (*ptr)(int) = foo;
	}

	#pragma test expect_ir(FOO_FUN C_STYLE_ASSIGN "{ var ref<foo_ptr_type,f,f> v0 = ref_var(ptr_of_function(foo)); }")
	{
		int (*ptr)(int) = &foo;
	}

	// CASTS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ var ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0; ptr_reinterpret(*v0, type_lit((int<4>)->real<4>)); }")
	{
		int(*ifFuncPtr)(float);
		(float(*)(int))ifFuncPtr;
	}
}
