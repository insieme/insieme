
int foo(int a) { return a; }

int main() {	

	// TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("decl ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0;")
	int(*ifFuncPtr)(float);

	#pragma test expect_ir("decl ref<ptr<()->unit,t,f>,f,f> v0;")
	void(*vvFuncPtr)(void);

	// EXPRESSIONS //////////////////////////////////////////////////////////////
	
	#define FOO_FUN "let foo_type = (int<4>) -> int<4>; let foo = lambda (int<4> a)->int<4> { return a; }; let foo_ptr_type = ptr<foo_type,t,f>; "
	#define BOOL_TO_INT "let bool_to_int = lambda (bool b) -> int<4> { if(b) return 1; else return 0; }; "
	#define C_STYLE_ASSIGN "let c_ass = lambda (ref<'a,f,'b> v1, 'a v2) -> 'a { v1 = v2; return *v1; };"
	{ }

	#pragma test expect_ir("{" FOO_FUN " ptr_of_function(foo); 1; }")
	{
		&foo;
		1; // to make INSPIRE different from next case
	}
	
	#pragma test expect_ir("{" FOO_FUN " ptr_of_function(foo); 2; }")
	{
		foo;
		2; // to make INSPIRE different from previous case
	}
	
	#pragma test expect_ir("{" FOO_FUN BOOL_TO_INT " decl ref<foo_ptr_type,f,f> v0; bool_to_int(!ptr_ne(*v0, ptr_null(type(foo_type), type(t), type(f)))); }")
	{
		int (*ptr)(int);
		!ptr;
	}
	
	#pragma test expect_ir("{" FOO_FUN " decl ref<foo_ptr_type,f,f> v0; ptr_deref(*v0)(5); }")
	{
		int (*ptr)(int);
		ptr(5);
	}
	
	#pragma test expect_ir("{" FOO_FUN C_STYLE_ASSIGN" decl ref<foo_ptr_type,f,f> v0; c_ass(v0, ptr_of_function(foo)); }")
	{
		int (*ptr)(int);
		ptr = foo;
	}
	
	#pragma test expect_ir("{" FOO_FUN C_STYLE_ASSIGN" decl ref<foo_ptr_type,f,f> v0; c_ass(v0, ptr_of_function(foo)); }")
	{
		int (*ptr)(int);
		ptr = &foo;
	}

	#pragma test expect_ir("{" FOO_FUN " ptr_deref(ptr_of_function(foo))(5); }")
	{
		(*(&foo))(5);
	}

	// INIT EXPRESSIONS ///////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{" FOO_FUN C_STYLE_ASSIGN" decl ref<foo_ptr_type,f,f> v0 = var(ptr_of_function(foo)); }")
	{
		int (*ptr)(int) = foo;
	}

	#pragma test expect_ir("{" FOO_FUN C_STYLE_ASSIGN" decl ref<foo_ptr_type,f,f> v0 = var(ptr_of_function(foo)); }")
	{
		int (*ptr)(int) = &foo;
	}

	// CASTS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ decl ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0; ptr_reinterpret(*v0, type((int<4>)->real<4>)); }")
	{
		int(*ifFuncPtr)(float);
		(float(*)(int))ifFuncPtr;
	}
	
	//int(*funcPtr)(int);
	//funcPtr = i_to_i;
	//funcPtr(1);

}
