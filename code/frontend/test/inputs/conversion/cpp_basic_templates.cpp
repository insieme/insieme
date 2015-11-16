
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
	#pragma test expect_ir(R"((v1 : uint<4>) -> uint<4> { return v1+num_cast(1, type_lit(uint<4>));	}(1u))")
	plusOne(1u);
	#pragma test expect_ir(R"((v1 : int<4>) -> int<4> { return v1+1; }(1))")
	plusOne(1);
	#pragma test expect_ir(R"((v1 : real<8>) -> real<8> { return v1+num_cast(1, type_lit(real<8>));	}(lit("1.0E+0":real<8>)))")
	plusOne(1.0);

	#pragma test expect_ir(R"(
	def fun003 = () -> int<4> { return 1; };
	def fun004 = () -> int<4> { return fun003()+fun003(); };
	def fun005 = () -> int<4> { return fun004()+fun003(); };
	def fun006 = () -> int<4> { return fun005()+fun004(); };
	fun006())")
	fib<4>();

	#pragma test expect_ir(R"(
	def fun004 = () -> int<4> { return 2; };
	def fun005 = () -> int<4> { return fun004()+1; };
	def fun006 = () -> int<4> { return fun005()+1; };
	fun006())")
	bla<2,3>();
	
	return 0;
}
