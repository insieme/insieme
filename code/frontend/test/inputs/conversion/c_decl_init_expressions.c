
int foo(int a) { return a; }

int main() {
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("var ref<char,f,f> v0 = ref_var(num_cast(lit(\"'a'\":int<4>), type_lit(char)));")
	char c = 'a';

	#pragma test expect_ir("var ref<uint<1>,f,f> v0 = ref_var(num_cast(5, type_lit(uint<1>)));")
	unsigned char uc = 5;
	#pragma test expect_ir("var ref<int<1>,f,f> v0 = ref_var(num_cast(2, type_lit(int<1>)));")
	signed char sc = 2;
	
	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var(2);")
	int i = 2;
	
	// QUALIFIERS ////////////////////////////////////////////////////////////// 
	
	#pragma test expect_ir("var ref<int<4>,t,f> v0 = ref_cast(ref_var(5), type_lit(t), type_lit(f), type_lit(plain));")
	const int ci = 5;
	
	#pragma test expect_ir("var ref<int<4>,f,t> v0 = ref_cast(ref_var(5), type_lit(f), type_lit(t), type_lit(plain));")
	volatile int vi = 5;
	
	#pragma test expect_ir("var ref<int<4>,t,t> v0 = ref_cast(ref_var(5), type_lit(t), type_lit(t), type_lit(plain));")
	const volatile int cvi = 5;
		
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ref_var(ptr_from_ref(v0)); }")
	{
		int i;
		int* pi = &i;
	}
	#pragma test expect_ir("{ var ref<int<4>,t,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ref_var(ptr_from_ref(v0)); }")
	{
		const int ci;
		const int* pci = &ci;
	}
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ref_var(ptr_cast(ptr_from_ref(v0), type_lit(t), type_lit(f))); }")
	{
		int i;
		const int* pci2 = &i;
	}

	// ARRAY TYPES /////////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var(array_create(type_lit(5), [1,2,3,4,5]));")
	int arr_all[5] = {1,2,3,4,5};
	
	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var(array_create(type_lit(5), [1,2]));")
	int arr_partial[5] = {1,2};
	
	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var(array_create(type_lit(5), [0]));")
	int arr_zero[5] = {0};
	
	#pragma test expect_ir("var ref<array<int<4>,3>,f,f> v0 = ref_var(array_create(type_lit(3), [0,1,2]));")
	int arr_implied[] = {0,1,2};
	
	#pragma test expect_ir("var ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"ref_var(array_create(type_lit(2), [array_create(type_lit(3), [1,2,3]), array_create(type_lit(3), [4,5,6])]));")
	int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

	#pragma test expect_ir("var ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"ref_var(array_create(type_lit(2), [array_create(type_lit(3), [1]), array_create(type_lit(3), [4,5])]));")
	int arr_multi_partial[2][3] = {{1}, {4,5}};
	
	// STRUCT TYPES //////////////////////////////////////////////////////////////

	// basic
	#pragma test expect_ir("alias s = struct { a: int<4>; b: real<4>; }; { var ref<s,f,f> v0 = ref_var(<s> {1, lit(\"1.0E+0\":real<4>)}); }")
	{ struct { int a; float b; } sif = { 1, 1.0f }; }

	// implicit
	#pragma test expect_ir("alias s = struct {a: int<4>; b: real<4>; c: uint<4>;}; { var ref<s,f,f> v0 = ref_var(<s> {1, 0.0f, 2u}); }")
	{ struct { int a; float b; unsigned c; } sifc = { .a = 1, .c = 2u }; }
	
	// explicit
	#pragma test expect_ir("alias s = struct {a: int<4>; b: real<4>; c: uint<4>;}; { var ref<s,f,f> v0 = ref_var(<s> {1, lit(\"0.0E+0\":real<4>), 2u}); }")
	{ struct { int a; float b; unsigned c; } sifc2 = { 1, 0.0f, 2u }; }
	
	// UNION TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("alias s = union {a: int<4>; b: real<4>;}; { var ref<s,f,f> v0 = ref_var(<s> {1}); }")
	{ union { int a; float b; } uif = { 1 }; }
	
	#pragma test expect_ir("alias s = union {a: int<4>; b: real<4>;}; { var ref<s,f,f> v0 = ref_var(<s> {1}); }")
	{ union { int a; float b; } uif = { .a = 1 }; }
	
	#pragma test expect_ir("alias s = union {a: int<4>; b: real<4>;}; { var ref<s,f,f> v0 = ref_var(<s> {lit(\"1.0E+0\":real<4>)}); }")
	{ union { int a; float b; } uif = { .b = 1.0f }; }
	
	// NESTED INITIALIZERS //////////////////////////////////////////////////////

	#pragma test expect_ir(R"( 
		alias ist = struct { inner1: int<4>; inner2: real<4>; }; 
		alias iut = union { u1: int<4>; u2: real<4>; }; 
		alias sut = struct { is: ist; iu: iut; }; 
		{ var ref<sut,f,f> v0 = ref_var(<sut> { <ist> { 1, lit("2.0E+0":real<4>) }, <iut> { 3 } } ); })")
	{ struct { struct { int inner1; float inner2; } is; union { int u1; float u2; } iu; } su = { { 1, 2.0f }, { 3 } }; }
	
	#pragma test expect_ir(R"(
		alias s = struct { a: int<4>; b: uint<4>; }; 
		{ var ref<array<s,2>,f,f> v0 = ref_var(array_create(type_lit(2), [<s> {1, 2u}, <s> {3, 4u}])); })")
	{ struct { int a; unsigned b; } su[2] = { { 1, 2u }, { 3, 4u } }; }

	// BOOL CONVERSION //////////////////////////////////////////////////////
	
	#define BOOL_TO_INT "def bool_to_int = (b: bool) -> int<4> { if(b) {return 1;} else {return 0;} };"

	#pragma test expect_ir(BOOL_TO_INT,"{","var ref<int<4>,f,f> v0 = ref_var(bool_to_int(1<2)); }")
	{ int boolconv = 1 < 2; }
}
