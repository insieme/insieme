
int main() {

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
	
	#pragma test expect_ir("{ decl ref<bool,f,f> v0 = var(false);  while(((!(*v0)) || (0!=0))) { v0 = true; { decl ref<int<4>,f,f> v1; } } }")
	do {
		int x;
	} while(0);
	
	#pragma test expect_ir("{ decl ref<int<4>,f,f> v0; switch(( *v0)) { case 0: { return 5; } case 4: { 5; 6; break; } case 5: { 6; break; } case 8: { return 6; } default: { break; } } }")
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
	
	{ }
	{ { } }
	;
	;;
		
	//int iii;
	//if(iii = 3) { }

	//{
	//	int i;
	//	for( ; ; ) {
	//		break;
	//	}
	//}

	//{
	//	int i = 0;
	//	for( ; i < 10; i += 1) {
	//		i;
	//	}
	//}

	//{
	//	int i = 0;
	//	for( ; ; i += 1) {
	//		break;
	//	}
	//}

	//for(int k = 0; k < 5; k++) {
	//	k;
	//}
	
	return 0;
}
