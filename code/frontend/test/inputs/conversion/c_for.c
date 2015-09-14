
int main() {
	#define C_STYLE_ASSIGN "let c_ass = lambda (ref<'a,f,'b> v1, 'a v2) -> 'a { v1 = v2; return *v1; };"
		
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; { c_ass(v0, 0); for(int<4> v1 = 0 .. 10 : 1) { v1; }; } }")
	{
		int i;
		for(i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); for(int<4> v1 = 2 .. 5 : 5) { v1; }; } }")
	{
		for(int k = 2; k < 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); for(int<4> v1 = 2 .. 5+1 : 5) { v1; }; } }")
	{
		for(int k = 2; k <= 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN R"({ decl ref<uint<4>,f,f> v0 = var(2u); for(uint<4> v1 = 2u .. 5u : 1u) { v1; }; } } )")
	{
		for(unsigned k = 2u; k < 5u; k++) k;
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); for(int<4> v1 = 2 .. 5 : 1) { }; } }")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); for(int<4> v1 = 2 .. 5 : 1) { }; } }")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	// check that we are doing nothing wrong here
	// (update once whileToFor is smarter!)	
	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); while(*v0>5) { c_ass(v0, *v0+1); }; } }")
	{
		for(int k = 2; k > 5; k+=1) { }
	}

	return 0;
}
