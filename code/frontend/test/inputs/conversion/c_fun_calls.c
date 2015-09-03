
float rec(float a) {
	if(a < 1.0f)
		return a;
	return rec(a-1.0f);
}

int i_to_i(int a) {
	return a;
}

int ii_to_i(int a, int b) {
	return b;
}

int main() {

	// TODO FE NG enable checks when call semantic is fixed
	
	//pragma test expect_ir("lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }(1)")
	i_to_i(1);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(1);
	
	//pragma test expect_ir("lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }( lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }(1))")
	i_to_i(i_to_i(1));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(i_to_i(1));
	
	//pragma test expect_ir("lambda (ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> int<4> { return *v2; }(1, 2)")
	ii_to_i(1,2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(1,2);
	
	//pragma test expect_ir("lambda (ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> int<4> { return *v2; }( lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }(1), 2)")
	ii_to_i(i_to_i(1),2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(1),2);
	
	//pragma test expect_ir("lambda (ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> int<4> { return *v2; }( lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }(1),",\
		"lambda (ref<int<4>,f,f> v1, ref<int<4>,f,f> v2) -> int<4> { return *v2; }( lambda (ref<int<4>,f,f> v1) -> int<4> { return *v1; }(1), 2))")
	ii_to_i(i_to_i(1),ii_to_i(i_to_i(1),2));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(1),ii_to_i(i_to_i(1),2));

	//pragma test expect_ir("recFun v0 { v0 = lambda (ref<int<4>,f,f> v1) -> int<4> { if(*v1<1) { return *v1; }; return v0(*v1-1); }; }(3)")
	rec(3);
	#pragma test expect_ir("EXPR_TYPE","real<4>")
	rec(3);

	return 0;
}
