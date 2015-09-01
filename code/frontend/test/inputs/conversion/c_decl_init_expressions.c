
int main() {
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<char,f,f> v0 = var(type_cast('a', type(char)));")
	char c = 'a';

	#pragma test expect_ir("decl ref<uint<1>,f,f> v0 = var(type_cast(5, type(uint<1>)));")
	unsigned char uc = 5;
	#pragma test expect_ir("decl ref<int<1>,f,f> v0 = var(type_cast(2, type(int<1>)));")
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

}
