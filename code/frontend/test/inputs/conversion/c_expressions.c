
int main () {
	
	// COMMA OPERATOR //////////////////////////////////////////////////////////////

	#pragma test expect_ir("lambda () -> int<4> { 2; return 3; }()")
	2, 3;

	#pragma test expect_ir("lambda () -> int<4> { lambda () -> int<4> { 2; return 3; }(); return 4; }()")
	2, 3, 4;
	
	#pragma test expect_ir("lambda () -> real<8> { 2; return lit(\"3.0E+0\":real<8>); }()")
	2, 3.0;
	
	#pragma test expect_ir("int_add(1, 2)")
	1 + 2;

	#pragma test expect_ir("int_sub(3, 4)")
	3 - 4;

	#pragma test expect_ir("int_mul(5, 6)")
	5 * 6;

	#pragma test expect_ir("int_div(7, 8)")
	7 / 8;
	
	#pragma test expect_ir("int_mod(9, 10)")
	9 % 10;

	#pragma test expect_ir("int_lshift(11, 12)")
	11 << 12;

	#pragma test expect_ir("int_rshift(13, 14)")
	13 >> 14;

	#pragma test expect_ir("int_and(15, 16)")
	15 & 16;

	#pragma test expect_ir("int_xor(17, 18)")
	17 ^ 18;

	#pragma test expect_ir("int_or(19, 20)")
	19 | 20;

	#pragma test expect_ir("decl ref<int<4>,f,f> v0; ")
	{
		int a = 0;
		a += 1;
	}
}
