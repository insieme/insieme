int main() {
	
	//===-------------------------------------------------------------------------------------------------------------------------------- UNARY OPERATORS ---===
	
	#define BOOL_TO_INT "let bool_to_int = lambda (bool b) -> int<4> { if(b) return 1; else return 0; };"

	#pragma test expect_ir("int_not(3)")
	~3;
	
	#pragma test expect_ir("{",BOOL_TO_INT,"bool_to_int(!(3!=0)); }")
	{ !3; }
	
	#pragma test expect_ir("3")
	+3;
	
	#pragma test expect_ir("-3")
	-3;
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v1 = var(0); ptr_from_ref(v1); }")
	{
		int x = 0;
		&x;
	}
	
	#pragma test expect_ir("{ decl ref<ptr<int<4>,f,f>,f,f> v0; *ptr_to_ref(*v0); }")
	{
		int* x;
		*x;
	}
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v1 = var(0); 0-v1; }")
	{
		int x = 0;
		-x;
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v1 = var(0); int_pre_inc(v1); }")
	{
		int v = 0;
		++v;
	}

	#pragma test expect_ir("{ decl ref<uint<2>,f,f> v1 = var(type_cast(0, type(uint<2>))); uint_post_inc(v1); }")
	{
		unsigned short v = 0;
		v++;
	}

	#pragma test expect_ir("{ decl ref<char,f,f> v1 = var(type_cast(0, type(char))); char_pre_dec(v1); }")
	{
		char v = 0;
		--v;
	}

	#pragma test expect_ir("{ decl ref<int<1>,f,f> v1 = var(type_cast(0, type(int<1>))); int_post_dec(v1); }")
	{
		signed char v = 0;
		v--;
	}
	
	//===------------------------------------------------------------------------------------------------------------------------------- BINARY OPERATORS ---===
	
	// COMMA OPERATOR //////////////////////////////////////////////////////////////
	
	#define C_STYLE_COMMA "let c_comma = lambda ('a lhs, 'b rhs) -> 'b { lhs; return rhs; };"

	#pragma test expect_ir("{",C_STYLE_COMMA,"c_comma(2, 3); }")
	{ 2, 3; }
	#pragma test expect_ir("EXPR_TYPE", "int<4>")
	2, 3;
	
	#pragma test expect_ir("{",C_STYLE_COMMA,"c_comma(c_comma(2, 3), 4); }")
	{ 2, 3, 4; }
	
	#pragma test expect_ir("{",C_STYLE_COMMA,"c_comma(2, lit(\"3.0E+0\":real<8>)); }")
	{ 2, 3.0; }
	#pragma test expect_ir("EXPR_TYPE", "real<8>")
	2, 3.0;
	
	// MATH //////////////////////////////////////////////////////////////

	#pragma test expect_ir("int_add(1, 2)")
	1 + 2;

	#pragma test expect_ir("int_sub(3, 4)")
	3 - 4;

	#pragma test expect_ir("int_mul(5, 6)")
	5 * 6;

	#pragma test expect_ir("int_div(7, 8)")
	7 / 8;
	
	#pragma test expect_ir("int_mod(9, 10)")
	9 % 10;
	
	// BITS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("int_lshift(11, 12)")
	11 << 12;

	#pragma test expect_ir("int_rshift(13, 14)")
	13 >> 14;

	#pragma test expect_ir("int_and(15, 16)")
	15 & 16;

	#pragma test expect_ir("int_xor(17, 18)")
	17 ^ 18;

	#pragma test expect_ir("int_or(19, 20)")
	19 | 20;

	// LOGICAL ////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("(0!=0) || (1!=0)")
	0 || 1;

	#pragma test expect_ir("(1!=0) && (0!=0)")
	1 && 0;
	
	// COMPARISON /////////////////////////////////////////////////////////

	#pragma test expect_ir("int_eq(1, 2)")
	1 == 2;

	#pragma test expect_ir("int_ne(1, 2)")
	1 != 2;
	
	#pragma test expect_ir("real_ne(lit(\"1.0E+0\":real<8>), lit(\"2.0E+0\":real<8>))")
	1.0 != 2.0;

	#pragma test expect_ir("int_lt(1, 2)")
	1 < 2;

	#pragma test expect_ir("int_gt(1, 2)")
	1 > 2;

	#pragma test expect_ir("int_le(1, 2)")
	1 <= 2;

	#pragma test expect_ir("int_ge(1, 2)")
	1 >= 2;

	// POINTER & ARRAYS ///////////////////////////////////////////////////////

	// one dimension

	#pragma test expect_ir("{ decl ref<array<int<4>,5>,f,f> v0; ref_deref(ptr_subscript(ptr_from_array(v0), 1)); }")
	{
		int a[5];
		a[1];
	}

	#pragma test expect_ir("{ decl ref<array<int<4>,5>,f,f> v0; ref_deref(ptr_subscript(ptr_from_array(v0), -1)); }")
	{
		int a[5];
		a[-1];
	}

	#pragma test expect_ir("{ decl ref<array<int<4>,5>,f,f> v0; ref_deref(ptr_subscript(ptr_from_array(v0), 1)); }")
	{
		int a[5];
		1[a];
	}
	
	#pragma test expect_ir("{ decl ref<array<int<4>,1>,f,f> v0; ref_deref(ptr_to_ref(ptr_from_array(v0))); }")
	{
		int a[1];
		*a;
	}

	#pragma test expect_ir("{ decl ref<ptr<int<4>,f,f>,f,f> v0; ptr_from_ref(v0); }")
	{
		int* a;
		&a;
	}
	
	#pragma test expect_ir("{ decl ref<array<int<4>,5>,f,f> v0; ptr_from_ref(v0); }")
	{
		int a[5];
		&a;
	}
	
	#pragma test expect_ir("{ decl ref<ptr<unit,f,f>,f,f> v0; ptr_add(*v0, 5); }")
	{
		void* a;
		a+5;
	}

	#pragma test expect_ir("{ decl ref<ptr<unit,f,f>,f,f> v0; ptr_add(*v0, 5); }")
	{
		void* a;
		5+a;
	}

	#pragma test expect_ir("{ decl ref<ptr<unit,f,f>,f,f> v0; ptr_post_inc(v0); ptr_post_dec(v0); ptr_pre_inc(v0); ptr_pre_dec(v0); }")
	{
		void* a;
		a++;
		a--;
		++a;
		--a;
	}
	
	#pragma test expect_ir("{ decl ref<ptr<unit,f,f>,f,f> v0; ptr_sub(*v0, 5); }")
	{
		void* a;
		a-5;
	}
	
	#pragma test expect_ir("{ decl ref<ptr<unit,f,f>,f,f> v0; ptr_gt(*v0,*v0); ptr_lt(*v0,*v0); ptr_le(*v0,*v0); ptr_ge(*v0,*v0); }")
	{
		void* a;
		a>a;
		a<a;
		a<=a;
		a>=a;
	}

	// multidimensional
	
	#pragma test expect_ir("{ decl ref<array<array<int<4>,3>,2>,f,f> v0; ref_deref(ptr_subscript(ptr_from_array(ptr_subscript(ptr_from_array(v0), 1)), 2)); }")
	{
		int a[2][3];
		a[1][2];
	}
	
	// note: there are no rvalue arrays in C!
	#pragma test expect_ir("{ decl ref<array<array<int<4>,3>,2>,f,f> v0; ptr_from_array(ptr_subscript(ptr_from_array(v0), 1)); }")
	{
		int a[2][3];
		a[1];
	}

	// COMPOUND //////////////////////////////////////////////////////////////

	#define C_STYLE_ASSIGN "let c_ass = lambda (ref<'a,f,'b> v1, 'a v2) -> 'a { v1 = v2; return *v1; };"

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1+1); }")
	{
		int a = 1;
		a += 1;
	}
	
	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1-2); }")
	{
		int a = 1;
		a -= 2;
	}
	
	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1/1); }")
	{
		int a = 1;
		a /= 1;
	}
	
	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1*5); }")
	{
		int a = 1;
		a *= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1%5); }")
	{
		int a = 1;
		a %= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1&5); }")
	{
		int a = 1;
		a &= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1|5); }")
	{
		int a = 1;
		a |= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, *v1^5); }")
	{
		int a = 1;
		a ^= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, int_lshift(*v1, 5)); }")
	{
		int a = 1;
		a <<= 5;
	}

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1 = var(1); c_ass(v1, int_rshift(*v1, 5)); }")
	{
		int a = 1;
		a >>= 5;
	}

	// ASSIGNMENT //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v1; c_ass(v1, 5); }")
	{
		int a;
		a = 5;
	}
	
	#pragma test expect_ir("{", C_STYLE_ASSIGN, "decl ref<int<4>,f,f> v0; decl ref<int<4>,f,f> v1; c_ass(v0, c_ass(v1, 1)); }")
	{
		int a, b;
		a = b = 1;
	}
	
	//===------------------------------------------------------------------------------------------------------------------------------- TERNARY OPERATOR ---===

	#pragma test expect_ir("(1!=0)?2:3")
	1?2:3;
	
	//===------------------------------------------------------------------------------------------------------------------------------------ MEMBER EXPR ---===
		
	#pragma test expect_ir("{ decl ref<struct{int<4> i},f,f> v0; *v0.i; }")
	{
		struct {
			int i;
		} ts;
		ts.i;
	}
	
	#pragma test expect_ir("{ decl ref<union{int<4> i},f,f> v0; *v0.i; }")
	{
		union {
			int i;
		} tu;
		tu.i;
	}

	#pragma test expect_ir("{ decl ref<ptr<struct{int<4> i},f,f>,f,f> v0;  *(ptr_to_ref(*v0).i); }")
	{
		struct {
			int i;
		} *ts;
		ts->i;
	}

	#pragma test expect_ir("{ decl ref<ptr<union{int<4> i},f,f>,f,f> v0;  *(ptr_to_ref(*v0).i); }")
	{
		union {
			int i;
		} *ts;
		ts->i;
	}
	
	//===---------------------------------------------------------------------------------------------------------------------------------- MISCELLANEOUS ---===
	
	#pragma test expect_ir("sizeof(type(real<8>))")
	sizeof(double);

	#pragma test expect_ir("sizeof(type(char))")
	sizeof(char);
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; sizeof(type(int<4>)); }")
	{
		int sizeof_int;
		sizeof(sizeof_int);
	}

	#pragma test expect_ir("{ decl ref<array<char,8>,f,f> v0; sizeof(type(array<char,8>)); }")
	{
		char char_arr[8];
		sizeof(char_arr);
	}

	// completely weird but possible compound init expr that can be assigned to
	typedef struct {
		unsigned data;
		int x, y;
	} Image;

	#pragma test expect_ir("STRING", "c_style_assignment(( var(struct{data:=0, x:=0, y:=0})), ( *( var(struct{data:=1, x:=1, y:=1}))))")
	(Image){0u, 0, 0} = (Image){1u,1,1};

	#pragma test expect_ir("STRING", "c_style_assignment((( var(struct{data:=0, x:=0, y:=0}))->x), 1)")
	(Image){0u, 0, 0}.x = 1;
}
