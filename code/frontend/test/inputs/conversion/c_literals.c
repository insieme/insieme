
int main() {

	#pragma test expect_ir("1")
	1;

	#pragma test expect_ir("-1")
	-1;

	#pragma test expect_ir("2u")
	2u;

	#pragma test expect_ir("3u")
	3U;

	#pragma test expect_ir("4l")
	4l;

	#pragma test expect_ir("5ul")
	5ul;

	#pragma test expect_ir("6ul")
	6UL;

	#pragma test expect_ir("7ull")
	7ull;

	#pragma test expect_ir("lit(\"8.0E+0\":real<4>)")
	8.0f;

	#pragma test expect_ir("lit(\"9.0E+0\":real<4>)")
	9.0F;

	#pragma test expect_ir("lit(\"1.0E+0\":real<8>)")
	1.0;

	// TODO: support long double types?
	//pragma test expect_ir("lit(\"2.0E+0\":real<16>)")
	//2.0L;

	#pragma test expect_ir("lit(\"3.0E+1\":real<8>)")
	3.0e+1;

	#pragma test expect_ir("lit(\"4.0E+1\":real<8>)")
	4.0E+1;

	#pragma test expect_ir("lit(\"'z'\":int<4>)")
	'z';

	#pragma test expect_ir("ptr_from_array(lit(\"abc\":ref<array<char,4>>))")
	"abc";

}
