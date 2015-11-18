
int rec(int a) {
	if(a < 1)
		return a;
	return rec(a-1);
}

int i_to_i(int a) {
	return a;
}

int ii_to_i(int a, int b) {
	return b;
}

int main() {

	// TODO FE NG enable checks when call semantic is fixed
	
	#pragma test expect_ir("(v1: int<4>) -> int<4> { return v1; }(1)")
	i_to_i(1);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(2); // use different number, otherwise same node as above -> pragma doesn't work
	
	#pragma test expect_ir("(v1: int<4>) -> int<4> { return v1; }( (v1: int<4>) -> int<4> { return v1; }(1))")
	i_to_i(i_to_i(1));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(i_to_i(2));
	
	#pragma test expect_ir("(v1: int<4>, v2: int<4>) -> int<4> { return v2; }(1, 2)")
	ii_to_i(1,2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(3,4);
	
	#pragma test expect_ir("(v1: int<4>, v2: int<4>) -> int<4> { return v2; }( (v1: int<4>) -> int<4> { return v1; }(1), 2)")
	ii_to_i(i_to_i(1),2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(3),4);
	
	#pragma test expect_ir("(v1: int<4>, v2: int<4>) -> int<4> { return v2; }( (v1: int<4>) -> int<4> { return v1; }(1),",\
		"(v1: int<4>, v2: int<4>) -> int<4> { return v2; }( (v1: int<4>) -> int<4> { return v1; }(1), 2))")
	ii_to_i(i_to_i(1),ii_to_i(i_to_i(1),2));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(2),ii_to_i(i_to_i(3),4));

	#pragma test expect_ir("decl IMP_rec: (int<4>)->int<4>; def IMP_rec = (v1: int<4>) -> int<4> { if(v1<1) { return v1; }; return IMP_rec(v1-1); }; { IMP_rec(3); }")
	{ rec(3); }
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	rec(4);

	return 0;
}
