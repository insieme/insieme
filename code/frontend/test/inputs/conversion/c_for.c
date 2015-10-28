
int main() {
	#define C_STYLE_ASSIGN "def c_ass = (v1: ref<'a,f,'b>, v2: 'a) -> 'a { v1 = v2; return *v1; };"
		
	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(0); for(int<4> v1 = 0 .. 10 : 1) { v1; }; } }")
	{
		for(int i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir(C_STYLE_ASSIGN "{{ var ref<int<4>,f,f> v0 = ref_var(2); for(int<4> v1 = 2 .. 5 : 5) { v1; }; }}")
	{
		for(int k = 2; k < 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir(C_STYLE_ASSIGN "{{ var ref<int<4>,f,f> v0 = ref_var(2); for(int<4> v1 = 2 .. 5+1 : 5) { v1; }; }}")
	{
		for(int k = 2; k <= 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir(C_STYLE_ASSIGN R"({{ var ref<uint<4>,f,f> v0 = ref_var(2u); for(uint<4> v1 = 2u .. 5u : 1u) { v1; }; } } )")
	{
		for(unsigned k = 2u; k < 5u; k++) k;
	}

	#pragma test expect_ir(C_STYLE_ASSIGN "{{ var ref<int<4>,f,f> v0 = ref_var(2); for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir(C_STYLE_ASSIGN "{{ var ref<int<4>,f,f> v0 = ref_var(2); for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f,plain> v0 =  var(2); while( *v0<5) { if( *v0==3) { c_ass(v0,*v0+1); continue; }; c_ass(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3){ continue; } }
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f,plain> v0 =  var(2); while( *v0<5) { if( *v0==3) { break; }; c_ass(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) break; }
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f,plain> v0 =  var(2); while( *v0<5) { if( *v0==3) { return 0; }; c_ass(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) return 0; }
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f,plain> v0 =  var(2); while( *v0<5) { gen_post_inc(v0); c_ass(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { k++; }
	}

	// check that we are doing nothing wrong here
	// (update once whileToFor is smarter!)	
	#pragma test expect_ir(C_STYLE_ASSIGN "{{ var ref<int<4>,f,f> v0 = ref_var(2); while(*v0>5) { c_ass(v0, *v0+1); }; }}")
	{
		for(int k = 2; k > 5; k+=1) { }
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f,plain> v0; { c_ass(v0, 0); while( *v0<5) { gen_post_inc(v0); }; }; { decl ref<int<4>,f,f,plain> v1 = var(0); for( int<4> v2 = 0 .. 5 : 1) { }; };    decl ref<int<4>,f,f,plain> v3 = var(*v0); }")	
	{
		int i;
		for(i=0; i<5; i++) { }
		for(int k = 0; k<5; k++) { }
		int z=i;
	}

	return 0;
}
