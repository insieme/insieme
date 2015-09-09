
int main() {
	
	// VARIABLE LENGTH ARRAY TYPES //////////////////////////////////////////////////////////////
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl int<inf> v1 = type_cast(*v0,type(int<inf>)); decl ref<array<real<4>,#v1>,f,f> v2; }")
	{
		int i = 3;
		float arrf[i];
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl int<inf> v1 = type_cast(*v0, type(int<inf>)); decl ref<real<4>,f,f> v2; decl ref<real<4>,f,f> v3; decl ref<array<real<4>,#v1>,f,f> v4; }")
	{
		int i = 3;
		float x,y,arrxy[i];
	}
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl int<inf> v1 = type_cast(*v0+3, type(int<inf>)); decl ref<array<real<4>,#v1>,f,f> v2; }")
	{
		int i = 3;
		float arrfm[i+3];
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl int<inf> v1 = type_cast(*v0, type(int<inf>)); decl ref<array<real<4>,#v1>,t,f> v2; }")
	{
		int i = 3;
		const float arrcf[i];
	}
	

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl int<inf> v1 = type_cast(*v0, type(int<inf>)); decl ref<array<real<4>,#v1>,f,t> v2; }")
	{
		int i = 3;
		volatile float arrvf[i];
	}
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl ref<int<4>,f,f> v1 = var(6); decl int<inf> v2 = type_cast(*v0, type(int<inf>)); decl int<inf> v3 = type_cast(*v1, type(int<inf>)); decl ref<array<array<real<4>,#v3>,#v2>,f,f> v4; }")
	{
		int i = 3;
		int j = 6;
		float arrarrf[i][j];
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl ref<int<4>,f,f> v1 = var(6); decl int<inf> v2 = type_cast(*v0+2, type(int<inf>)); decl int<inf> v3 = type_cast(*v1+5, type(int<inf>)); decl ref<array<array<real<4>,#v3>,#v2>,f,f> v4; }")
	{
		int i = 3;
		int j = 6;
		float arrarrfmm[i+2][j+5];
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl ref<int<4>,f,f> v1 = var(6); decl ref<int<4>,f,f> v2 = var(10); decl int<inf> v3 = type_cast(*v0, type(int<inf>)); decl int<inf> v4 = type_cast(*v1, type(int<inf>)); decl int<inf> v5 = type_cast(*v2, type(int<inf>)); decl ref<array<array<array<real<4>,#v5>,#v4>,#v3>,f,f> v6; }")
	{
		int i = 3;
		int j = 6;
		int k = 10;
		float arrarrarrf[i][j][k];
	}
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(3); decl ref<int<4>,f,f> v1 = var(6); decl ref<int<4>,f,f> v2 = var(10); decl int<inf> v3 = type_cast(*v0+2, type(int<inf>)); decl int<inf> v4 = type_cast(*v1, type(int<inf>)); decl int<inf> v5 = type_cast(*v2+1, type(int<inf>)); decl ref<array<array<array<real<4>,#v5>,#v4>,#v3>,f,f> v6; }")
	{
		int i = 3;
		int j = 6;
		int k = 10;
		float arrarrarrfm[i+2][j][k+1];
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(61); decl int<inf> v1 = type_cast(10, type(int<inf>)); decl int<inf> v2 = type_cast(*v0+2, type(int<inf>)); decl ref<array<array<array<real<4>,10>,#v2>,#v1>,t,t> v3; }")
	{
		int j = 61;
		const volatile float arrarrarrffm[10][j+2][10];
	}
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(4); decl int<inf> v1 = type_cast(*v0,type(int<inf>)); decl ref<array<ptr<real<4>,f,f>,#v1>,f,f> v2; }")
	{
		int i = 4;
		float* arrpf[i];
	}
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(4); decl int<inf> v1 = type_cast(*v0,type(int<inf>)); decl ref<array<ptr<real<4>,t,f>,#v1>,f,f> v2; }")
	{
		int i = 4;
		const float* arrpcf[i];
	}
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(4); decl int<inf> v1 = type_cast(*v0,type(int<inf>)); decl ref<array<ptr<real<4>,t,f>,#v1>,f,t> v2; }")
	{
		int i = 4;
		const float *volatile arrvpcf[i];
	}

	// VARIABLE LENGTH ARRAY SIZEOF //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; sizeof(type(int<4>))*num_cast(*v0, type(uint<8>)); }")
	{
		int i;
		sizeof(int[i]);
	}
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; decl ref<int<4>,f,f> v1; sizeof(type(array<int<4>,2>))*num_cast(*v0, type(uint<8>))*num_cast(5, type(uint<8>))*num_cast(*v1, type(uint<8>)); }")
	{
		int i, j;
		sizeof(int[i][5][j][2]);
	}

	int i=10;
	int j=20;
	int k[j];
	int l[i][j];
	//TODO: implement UnaryExprOrTypeTraitExpr for VLAs
	//sizeof(k);
	//sizeof(l);
	
}
