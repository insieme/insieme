


int a(int b, float c) {
	return 0;
}

int main() {	
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "decl ref<char,f,f> v0;"
	char c;
	#pragma test expected "decl ref<uint<1>,f,f> v0;"
	unsigned char uc;
	#pragma test expected "decl ref<int<1>,f,f> v0;"
	signed char sc;
	
	#pragma test expected "decl ref<int<2>,f,f> v0;"
	short ss;
	#pragma test expected "decl ref<uint<2>,f,f> v0;"
	unsigned short us;
	
	#pragma test expected "decl ref<int<4>,f,f> v0;"
	int m;
	#pragma test expected "decl ref<int<4>,t,f> v0;"
	const int y;
	#pragma test expected "decl ref<int<4>,f,t> v0;"
	volatile int z;
	#pragma test expected "decl ref<int<4>,t,t> v0;"
	const volatile int q;
	
	#pragma test expected "decl ref<real<4>,f,f> v0;"
	float f1;
	#pragma test expected "decl ref<real<8>,f,f> v0;"
	double f2;
	
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "decl ref<ptr<int<4>,f,f>,f,f> v0;"
	int* pi;
	#pragma test expected "decl ref<ptr<int<4>,t,f>,f,f> v0;"
	const int* pci;
	#pragma test expected "decl ref<ptr<int<4>,f,t>,f,f> v0;"
	volatile int* pvi;
	#pragma test expected "decl ref<ptr<int<4>,t,t>,f,f> v0;"
	const volatile int* pcvi;
	
	#pragma test expected "decl ref<ptr<int<4>,f,f>,t,f> v0;"
	int *const cpi;
	#pragma test expected "decl ref<ptr<int<4>,t,f>,t,f> v0;"
	const int *const cpci;
	
	#pragma test expected "decl ref<ptr<ptr<int<4>,f,f>,f,f>,f,f> v0;"
	int** ppi;
	#pragma test expected "decl ref<ptr<ptr<ptr<int<4>,f,f>,f,f>,t,f>,f,f> v0;"
	int * *const * ppcpi;
	
	// ARRAY TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "decl ref<array<int<4>,2>,f,f> v0;"
	int array[2];
}
