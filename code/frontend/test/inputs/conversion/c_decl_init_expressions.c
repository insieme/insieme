
int main() {
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<char,f,f> v0 = var(num_cast(lit(\"'a'\":int<4>), type(char)));")
	char c = 'a';

	#pragma test expect_ir("decl ref<uint<1>,f,f> v0 = var(num_cast(5, type(uint<1>)));")
	unsigned char uc = 5;
	#pragma test expect_ir("decl ref<int<1>,f,f> v0 = var(num_cast(2, type(int<1>)));")
	signed char sc = 2;
	
	#pragma test expect_ir("decl ref<int<4>,f,f> v0 = var(2);")
	int i = 2;
	
	// QUALIFIERS ////////////////////////////////////////////////////////////// 
	
	#pragma test expect_ir("decl ref<int<4>,t,f> v0 = ref_cast(var(5), type(t), type(f));")
	const int ci = 5;
	
	#pragma test expect_ir("decl ref<int<4>,f,t> v0 = ref_cast(var(5), type(f), type(t));")
	volatile int vi = 5;
	
	#pragma test expect_ir("decl ref<int<4>,t,t> v0 = ref_cast(var(5), type(t), type(t));")
	const volatile int cvi = 5;
		
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; decl ref<ptr<int<4>,f,f>,f,f> v1 = var(ptr_from_ref(v0)); }")
	{
		int i;
		int* pi = &i;
	}
	#pragma test expect_ir("{ decl ref<int<4>,t,f> v0; decl ref<ptr<int<4>,t,f>,f,f> v1 = var(ptr_from_ref(v0)); }")
	{
		const int ci;
		const int* pci = &ci;
	}
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; decl ref<ptr<int<4>,t,f>,f,f> v1 = var(ptr_cast(ptr_from_ref(v0), type(t), type(f))); }")
	{
		int i;
		const int* pci2 = &i;
	}

	// ARRAY TYPES /////////////////////////////////////////////////////////////////

	#pragma test expect_ir("decl ref<array<int<4>,5>,f,f> v0 = var(array_create(type(int<4>), type(5), [1,2,3,4,5]));")
	int arr_all[5] = {1,2,3,4,5};
	
	#pragma test expect_ir("decl ref<array<int<4>,5>,f,f> v0 = var(array_create(type(int<4>), type(5), [1,2]));")
	int arr_partial[5] = {1,2};
	
	#pragma test expect_ir("decl ref<array<int<4>,5>,f,f> v0 = var(array_create(type(int<4>), type(5), [0]));")
	int arr_zero[5] = {0};
	
	#pragma test expect_ir("decl ref<array<int<4>,3>,f,f> v0 = var(array_create(type(int<4>), type(3), [0,1,2]));")
	int arr_implied[] = {0,1,2};
	
	#pragma test expect_ir("decl ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"var(array_create(type(array<int<4>,3>), type(2), [array_create(type(int<4>), type(3), [1,2,3]), array_create(type(int<4>), type(3), [4,5,6])]));")
	int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

	#pragma test expect_ir("decl ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"var(array_create(type(array<int<4>,3>), type(2), [array_create(type(int<4>), type(3), [1]), array_create(type(int<4>), type(3), [4,5])]));")
	int arr_multi_partial[2][3] = {{1}, {4,5}};
	
	// STRUCT TYPES //////////////////////////////////////////////////////////////

	// basic
	#pragma test expect_ir("{ let s = struct {int<4> a; real<4> b}; decl ref<s,f,f> v0 = var(struct s{1, lit(\"1.0E+0\":real<4>)}); }")
	{ struct { int a; float b; } sif = { 1, 1.0f }; }

	// implicit
	#pragma test expect_ir("{ let s = struct {int<4> a; real<4> b; uint<4> c}; decl ref<s,f,f> v0 = var(struct s{1, 0.0f, 2u}); }")
	{ struct { int a; float b; unsigned c; } sifc = { .a = 1, .c = 2u }; }
	
	// explicit
	#pragma test expect_ir("{ let s = struct {int<4> a; real<4> b; uint<4> c}; decl ref<s,f,f> v0 = var(struct s{1, lit(\"0.0E+0\":real<4>), 2u}); }")
	{ struct { int a; float b; unsigned c; } sifc2 = { 1, 0.0f, 2u }; }
	
	// UNION TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{ let s = union {int<4> a; real<4> b}; decl ref<s,f,f> v0 = var(union s{a = 1}); }")
	{ union { int a; float b; } uif = { 1 }; }
	
	#pragma test expect_ir("{ let s = union {int<4> a; real<4> b}; decl ref<s,f,f> v0 = var(union s{a = 1}); }")
	{ union { int a; float b; } uif = { .a = 1 }; }
	
	#pragma test expect_ir("{ let s = union {int<4> a; real<4> b}; decl ref<s,f,f> v0 = var(union s{b = lit(\"1.0E+0\":real<4>)}); }")
	{ union { int a; float b; } uif = { .b = 1.0f }; }
	
	// NESTED INITIALIZERS //////////////////////////////////////////////////////

	#pragma test expect_ir("{ let ist = struct { int<4> inner1; real<4> inner2 }; let iut = union { int<4> u1; real<4> u2 }; let sut = struct { ist is; iut iu; }; decl ref<sut,f,f> v0 = var(struct sut { struct ist { 1, lit(\"2.0E+0\":real<4>) }, union iut { u1 = 3 } } ); }")
	{ struct { struct { int inner1; float inner2; } is; union { int u1; float u2; } iu; } su = { { 1, 2.0f }, { 3 } }; }
	
	#pragma test expect_ir("{ let s = struct { int<4> a; uint<4> b }; decl ref<array<s,2>,f,f> v0 = var(array_create(type(s), type(2), [struct s {1, 2u}, struct s{3, 4u}])); }")
	{ struct { int a; unsigned b; } su[2] = { { 1, 2u }, { 3, 4u } }; }

}