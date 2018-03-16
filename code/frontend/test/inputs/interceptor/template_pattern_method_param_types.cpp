#include "template_pattern_method_param_types.h"


int main() {
	int magic;

	#pragma test expect_ir(R"({
		var ref<IMP_A<1>,f,f,plain> v0 = lit("IMP_A::ctor" : IMP_A<1>::())(ref_decl(type_lit(ref<IMP_A<1>,f,f,plain>)));
		var ref<IMP_B<1,2>,f,f,plain> v1 = instantiate_ctor(lit("target_type" : IMP_B<1,2>::(ref<IMP_A<1>,t,f,cpp_ref>)), lit("IMP_B::ctor" : IMP_B<'T_0_0,'T_0_1>::(ref<IMP_A<'TX_0>,t,f,cpp_ref>)))(ref_decl(type_lit(ref<IMP_B<1,2>,f,f,plain>)), ref_kind_cast(v0, type_lit(cpp_ref)));
		var ref<IMP_A<2>,f,f,plain> v2 = lit("IMP_A::ctor" : IMP_A<2>::())(ref_decl(type_lit(ref<IMP_A<2>,f,f,plain>)));
		var ref<IMP_B<1,2>,f,f,plain> v3 = instantiate_ctor(lit("target_type" : IMP_B<1,2>::(ref<IMP_A<2>,t,f,cpp_ref>)), lit("IMP_B::ctor" : IMP_B<'T_0_0,'T_0_1>::(ref<IMP_A<'TX_0>,t,f,cpp_ref>)))(ref_decl(type_lit(ref<IMP_B<1,2>,f,f,plain>)), ref_kind_cast(v2, type_lit(cpp_ref)));
	})")
	{
		A<1> a1;
		B<1,2> b1(a1);

		A<2> a2;
		B<1,2> b2(a2);
	}
}
