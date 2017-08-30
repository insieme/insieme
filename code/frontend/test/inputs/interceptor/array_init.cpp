
#include "interceptor_header.h"

struct A {};

int main() {
	int magic;

	#pragma test expect_ir(R"(
	def struct IMP_A {
	};
	{
		var ref<array<IMP_A,20u>,f,f,plain> v0 =
			<ref<array<IMP_A,20u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_A,20u>,f,f,plain>))) {};
		var ref<array<IMP_ns_colon__colon_S,20u>,f,f,plain> v1 =
			<ref<array<IMP_ns_colon__colon_S,20u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_ns_colon__colon_S,20u>,f,f,plain>))) {};
	}
	)")
	{
		A arr[20];
		ns::S arrInter[20];
	}
}
