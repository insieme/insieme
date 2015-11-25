
template<typename T>
T plusOne(T v) {
	return v+1;
}

template<int val>
int fib() { return fib<val-1>() + fib<val-2>(); }
template<>
int fib<0>() { return 1; }
template<>
int fib<1>() { return 1; }

template<int m, int n>
int bla() { return bla<m-1,n-1>()+1; }
template<>
int bla<0,1>() { return 2; }

int main() {
	;

	#pragma test expect_ir(R"(
		def IMP_plusOne_unsigned_int_returns_unsigned_int = (v1 : uint<4>) -> uint<4> { return v1+num_cast(1, type_lit(uint<4>)); }; 
		IMP_plusOne_unsigned_int_returns_unsigned_int(1u))")
	plusOne(1u);
	#pragma test expect_ir(R"(
		def IMP_plusOne_int_returns_int = (v1 : int<4>) -> int<4> { return v1+1; }; 
		IMP_plusOne_int_returns_int(1))")
	plusOne(1);
	#pragma test expect_ir(R"(
		def IMP_plusOne_double_returns_double = (v1 : real<8>) -> real<8> { return v1+num_cast(1, type_lit(real<8>)); }; 
		IMP_plusOne_double_returns_double(lit("1.0E+0":real<8>)))")
	plusOne(1.0);

	#pragma test expect_ir(R"(
	def IMP_fib_0 = () -> int<4> { return 1; };
	def IMP_fib_1 = () -> int<4> { return 1; };
	def IMP_fib_2_returns_int = () -> int<4> { return IMP_fib_1()+IMP_fib_0(); };
	def IMP_fib_3_returns_int = () -> int<4> { return IMP_fib_2_returns_int()+IMP_fib_1(); };
	def IMP_fib_4_returns_int = () -> int<4> { return IMP_fib_3_returns_int()+IMP_fib_2_returns_int(); };
	IMP_fib_4_returns_int())")
	fib<4>();

	#pragma test expect_ir(R"(
	def IMP_bla_0_1 = () -> int<4> { return 2; };
	def IMP_bla_1_2_returns_int = () -> int<4> { return IMP_bla_0_1()+1; };
	def IMP_bla_2_3_returns_int = () -> int<4> { return IMP_bla_1_2_returns_int()+1; };
	IMP_bla_2_3_returns_int())")
	bla<2,3>();
	
	return 0;
}
