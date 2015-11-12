
template<typename T>
T plusOne(T v) {
	return v+1;
}

int main() {
	#pragma test expect_ir(R"((v1 : uint<4>) -> uint<4> { return v1+num_cast(1, type_lit(uint<4>));	}(1u))")
	plusOne(1u);
	#pragma test expect_ir(R"((v1 : int<4>) -> int<4> { return v1+1; }(1))")
	plusOne(1);
	#pragma test expect_ir(R"((v1 : real<8>) -> real<8> { return v1+num_cast(1, type_lit(real<8>));	}(lit("1.0E+0":real<8>)))")
	plusOne(1.0);
	return 0;
}
