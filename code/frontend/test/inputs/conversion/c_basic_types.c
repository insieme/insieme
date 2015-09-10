
int main() {
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<char,f,f> v0;")
	char c;

	#pragma test expect_ir("decl ref<uint<1>,f,f> v0;")
	unsigned char uc;
	#pragma test expect_ir("decl ref<int<1>,f,f> v0;")
	signed char sc;
	
	#pragma test expect_ir("decl ref<int<2>,f,f> v0;")
	short ss;
	#pragma test expect_ir("decl ref<uint<2>,f,f> v0;")
	unsigned short us;
	
	#pragma test expect_ir("decl ref<int<4>,f,f> v0;")
	int m;
	#pragma test expect_ir("decl ref<int<4>,t,f> v0;")
	const int y;
	#pragma test expect_ir("decl ref<int<4>,f,t> v0;")
	volatile int z;
	#pragma test expect_ir("decl ref<int<4>,t,t> v0;")
	const volatile int q;
	
	#pragma test expect_ir("decl ref<real<4>,f,f> v0;")
	float f1;
	#pragma test expect_ir("decl ref<real<8>,f,f> v0;")
	double f2;
	
	// BASE TYPE TYPEDEFS //////////////////////////////////////////////////////////////
	
	typedef float fluffy;
	typedef volatile float fluffier;
	
	#pragma test expect_ir("decl ref<real<4>,f,f> v0;")
	fluffy fluff;
	
	#pragma test expect_ir("decl ref<real<4>,f,t> v0;")
	fluffier fluffer;
	
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<ptr<int<4>,f,f>,f,f> v0;")
	int* pi;
	#pragma test expect_ir("decl ref<ptr<int<4>,t,f>,f,f> v0;")
	const int* pci;
	#pragma test expect_ir("decl ref<ptr<int<4>,f,t>,f,f> v0;")
	volatile int* pvi;
	#pragma test expect_ir("decl ref<ptr<int<4>,t,t>,f,f> v0;")
	const volatile int* pcvi;
	
	#pragma test expect_ir("decl ref<ptr<int<4>,f,f>,t,f> v0;")
	int *const cpi;
	#pragma test expect_ir("decl ref<ptr<int<4>,t,f>,t,f> v0;")
	const int *const cpci;
	
	#pragma test expect_ir("decl ref<ptr<ptr<int<4>,f,f>,f,f>,f,f> v0;")
	int** ppi;
	#pragma test expect_ir("decl ref<ptr<ptr<ptr<int<4>,f,f>,f,f>,t,f>,f,f> v0;")
	int * *const * ppcpi;
	
	// FIXED SIZE ARRAY TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<array<real<4>,2>,f,f> v0;")
	float arrf[2];
	#pragma test expect_ir("decl ref<array<real<4>,2>,t,f> v0;")
	const float arrcf[2];
	#pragma test expect_ir("decl ref<array<real<4>,2>,f,t> v0;")
	volatile float arrvf[2];
	
	#pragma test expect_ir("decl ref<array<array<real<4>,5>,2>,f,f> v0;")
	float arrarrf[2][5];
	#pragma test expect_ir("decl ref<array<array<array<real<4>,3>,5>,2>,f,f> v0;")
	float arrarrarrf[2][5][3];
	
	#pragma test expect_ir("decl ref<array<ptr<real<4>,f,f>,2>,f,f> v0;")
	float* arrpf[2];
	#pragma test expect_ir("decl ref<array<ptr<real<4>,t,f>,2>,f,f> v0;")
	const float* arrpcf[2];
	#pragma test expect_ir("decl ref<array<ptr<real<4>,t,f>,2>,f,t> v0;")
	const float *volatile arrvpcf[2];
	
	// ENUM TYPES //////////////////////////////////////////////////////////////
		
	typedef enum { Bla, Alb } enum_t;
	#pragma test expect_ir("REGEX", R"(decl ref<__insieme_enum<\w+enum_t\w+,Bla,Alb>,f,f> v0 = .*)")
	enum_t enu;
	
	#pragma test expect_ir("REGEX", R"(decl ref<__insieme_enum<\w+,XY,ZR>,f,f> v0 = .*)")
	enum { XY, ZR } bla; 

	// STRUCT TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("decl ref<struct{int<4> i},f,f> v0;")
	struct { int i; } swi_anon;
	
	typedef struct swi_s { int i; } swi_t;
	#pragma test expect_ir("REGEX", R"(decl ref<struct \w+swi_s\w+ <i:int<4>>,f,f> v0 = .*)")
	swi_t swi_1;
	#pragma test expect_ir("REGEX", R"(decl ref<struct \w+swi_s\w+ <i:int<4>>,f,f> v0 = .*)")
	struct swi_s swi_2; 
	
	typedef union { int i; } union_t;
	#pragma test expect_ir("REGEX", R"(decl ref<union \w+ <i:int<4>>,f,f> v0 = .*)")
	union_t uni;

	// FUNCTION POINTER TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("decl ref<ptr<(real<4>)->int<4>,f,f>,f,f> v0;")
	int(*ifFuncPtr)(float);

	#pragma test expect_ir("decl ref<ptr<()->unit,f,f>,f,f> v0;")
	void(*vvFuncPtr)(void);
}
