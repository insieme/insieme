
void unusedUnnamedFunArg(int, float) {

}

int main() {

	int magic;

	#pragma test expect_ir(R"(
		def IMP_unusedUnnamedFunArg = function(v0 : ref<int<4>, f, f, plain>, v1 : ref<real<4>, f, f, plain>) -> unit { };
		{
			IMP_unusedUnnamedFunArg(5, 4.0E+0f);
		}
	)")
	{
		unusedUnnamedFunArg(5, 4.0f);
	}

}
