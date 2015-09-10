
int main() {

	// TODO FE NG new call semantic
	#define C_STYLE_ASSIGN "let c_ass = lambda (ref<'a,f,'b> v1, 'a v2) -> 'a { v1 = v2; return *v1; };"

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; decl ref<int<4>,f,f> v1;}")
	{
		int x, y;
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; decl ref<ptr<int<4>,f,f>,f,f> v1;}")
	{
		int x, *y;
	}
	
	#pragma test expect_ir("if(int_ne(1, 0)) { decl ref<int<4>,f,f> v0; }")
	if(1) {
		int a;
	} 
	
	#pragma test expect_ir("if(int_ne(1, 0)) { decl ref<int<4>,f,f> v0; } else { decl ref<real<4>,f,f> v0; }")
	if(1) {
		int a;
	} else {
		float b;
	}
	
	#pragma test expect_ir("if(int_ne(1, 0)) { decl ref<int<4>,f,f> v0; } else { if(int_ne(0, 0)) { decl ref<real<4>,f,f> v0; } }")
	if(1) {
		int a;
	} else if(0) {
		float b;
	}
	
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; if(int_ne(c_ass(v0, 1), 0)) { } }")
	{
		int i;
		if(i = 1) { }
	}

	#pragma test expect_ir("while(int_ne(0, 0)) { decl ref<real<8>,f,f> v0; }")
	while(0) {
		double a;
	}
	
	#pragma test expect_ir("while(int_ne(0, 0)) { break; }")
	while(0) {
		break;
	}
	
	#pragma test expect_ir("while(int_ne(0, 0)) { continue; }")
	while(0) {
		continue;
	}
	
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; while(int_ne(c_ass(v0, 1), 0)) { break; } }")
	{
		int i;
		while(i = 1) { break; }
	}

	#pragma test expect_ir("{ decl ref<bool,f,f> v0 = var(false);  while(!*v0 || 0!=0) { v0 = true; { decl ref<int<4>,f,f> v1; } } }")
	do {
		int x;
	} while(0);
	
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; { decl ref<bool,f,f> v1 = var(false); while(!*v1 || int_ne(c_ass(v0, 1), 0)) { v1 = true; { break; } } } }")
	{
		int i;
		do {
			break;
		} while(i = 1);
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; switch(*v0) { case 0: { return 5; } case 4: { 5; 6; break; } case 5: { 6; break; } case 8: { return 6; } default: { break; } } }")
	{
		int a;
		switch(a) {
			case 0: return 5;
			case 4: 5; 
			case 5: 6; break;
			case 8: return 6;
			default: break;
		}
	}
	
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; switch(c_ass(v0, 0)) { case 0: { return 5; } case 4: { 5; 6; break; } case 5: { 6; break; } case 8: { return 6; } default: { break; } } }")
	{
		int a;
		switch(a=0) {
			case 0: return 5;
			case 4: 5;
			case 5: 6; break;
			case 8: return 6;
			default: break;
		}
	}

	{ }
	{ { } }
	;
	;;
		
	#pragma test expect_ir("{" C_STYLE_ASSIGN "decl ref<int<4>,f,f> v0; { c_ass(v0, 0); while(*v0 < 10) { *v0; int_post_inc(v0); } } }")
	{
		int i;
		for(i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); while(*v0 < 5) { *v0; c_ass(v0, *v0+1); } } }")
	{
		for(int k = 2; k < 5; k+=1) {
			k;
		}
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); while(*v0 < 5) { *v0; c_ass(v0, *v0+1); } } }")
	{
		for(int k = 2; k < 5; k+=1) k;
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); while(*v0 < 5) { c_ass(v0, *v0+1); } } }")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("{" C_STYLE_ASSIGN "{ decl ref<int<4>,f,f> v0 = var(2); while(*v0 < 5) { c_ass(v0, *v0+1); } } }")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(0); { while(*v0 < 10) { *v0; int_post_inc(v0); } } }")
	{
		int i = 0;
		for(; i < 10; i++) {
			i;
		}
	}

    #pragma test expect_ir("{ decl ref<int<4>,f,f> v0 = var(0); { while(true) { break; int_post_inc(v0); } } }")
	{
		int i = 0;
		for(;; i++) {
			break;
		}
	}

    #pragma test expect_ir("{ { while(true) { break; } } }")
	{
		for(;;) {
			break;
		}
	}

	#pragma test expect_ir("{ { decl ref<int<4>,f,f> v0 = var(0); while(*v0 < 0); } }")
	{
		for(int i = 0; i < 0;) {
		}
	}

	return 0;
}
